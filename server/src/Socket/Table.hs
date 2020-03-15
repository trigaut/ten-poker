{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Socket.Table where
import           Control.Concurrent      hiding ( yield )
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Data.Map.Lazy                 as M
import           Database.Persist.Postgresql
import qualified Network.WebSockets            as WS
import           Prelude

import           Database.Persist
import           Database.Persist.Postgresql    ( ConnectionString
                                                , SqlPersistT
                                                , runMigration
                                                , withPostgresqlConn
                                                )
import           Types
import           Control.Lens
import           Control.Lens            hiding ( Fold )
import           Poker.Types             hiding ( LeaveSeat )

import qualified Data.ByteString.Lazy.Char8    as C
import           Control.Monad.Except
import           Control.Monad.Reader


import           System.Random
import qualified Data.Map.Lazy                 as M

import           Poker.Game.Blinds
import           Poker.Game.Game
import           Poker.Types                    ( Player )
import           Data.Maybe
import           Poker.Game.Utils
import           Socket.Types
import           Socket.Utils
import           System.Random
import           Poker.Poker
import           Data.ByteString.UTF8           ( fromString )

import           Database
import           Schema

import           Pipes.Aeson
import           Pipes                   hiding ( next )
import           Pipes.Core                     ( push )
import           Pipes.Concurrent
import           Pipes.Lift                     ( runReaderP )
import           Pipes.Parse             hiding ( decode
                                                , encode
                                                , next
                                                )
import qualified Pipes.Prelude                 as P
import           Poker.Game.Privacy


data GameEnv = GameEnv
  { _envConnStr :: ConnectionString
  , _envServerState :: TVar ServerState
  , _envTableName :: TableName
  , _envGameOutMailbox :: Input Game
  , _envGameInMailbox:: Output Game
  }

makeLenses ''GameEnv

type GamePipeline a = Pipe Game Game (ReaderT GameEnv IO) a

setUpTablePipes
  :: ConnectionString -> TVar ServerState -> TableName -> Table -> IO (Async ())
setUpTablePipes _envConnStr _envServerState _envTableName Table {..} = do
  async $ forever $ runEffect $ gamePipeline GameEnv
    { _envGameInMailbox  = gameInMailbox
    , _envGameOutMailbox = gameOutMailbox
    , ..
    }

-- this is the pipeline of effects we run everytime a new game state
-- is placed in the tables
-- incoming mailbox for new game states.
--
-- New game states are send to the table's incoming mailbox every time a player acts
-- in a way that follows the game rules
--
-- Delays with "pause" at the end of each game stage (Flop, River etc) for UX
-- are done client side.
gamePipeline :: GameEnv -> Effect IO ()
gamePipeline env =
  do
      gameProducer
    >-> broadcast env
    >-> logGame env
    >-> updateTable env
    >-> writeGameToDB env
    >-> nextStagePause
    >-> timePlayer env
    >-> progress env
  where gameProducer = runReaderP env $ fromInput (env ^. envGameOutMailbox)

-- Delay to enhance UX based on game stages
timePlayer :: GameEnv -> Pipe Game Game IO ()
timePlayer GameEnv {..} = do
  g@Game {..} <- await
  let currPlyrToAct = ((!!) (getGamePlayerNames g)) <$> _currentPosToAct
  liftIO $ forM_ currPlyrToAct $ runPlayerTimer _envServerState _envTableName g
  yield g
 where
  -- We watch incoming game states. We compare the initial gamestates
  -- with the game state when the timer ends.
  -- If the state is still the same then we timeout the player to act
  -- to force the progression of the game.
  runPlayerTimer
    :: TVar ServerState -> TableName -> Game -> PlayerName -> IO (Async ())
  runPlayerTimer s tableName gameWhenTimerStarts plyrName = async $ do
    threadDelay $ 3 * 1000000 -- 30 seconds
    mbTable <- atomically $ getTable s tableName
    case mbTable of
      Nothing         -> return ()
      Just Table {..} -> do
        let gameHasNotProgressed = gameWhenTimerStarts == game
            playerStillHasToAct  = doesPlayerHaveToAct plyrName game
        when (gameHasNotProgressed && playerStillHasToAct)
          $ case runPlayerAction game timeoutAction of
              Left err -> print err
              Right progressedGame ->
                runEffect $ yield progressedGame >-> toOutput gameInMailbox
    where timeoutAction = PlayerAction { name = plyrName, action = Timeout }


-- Delay to enhance UX so game doesn't move through stages
-- instantly when no players can act i.e everyone all in.
nextStagePause :: Pipe Game Game IO ()
nextStagePause = do
  g <- await
  when (canProgressGame g) $ liftIO $ threadDelay $ pauseDuration g
  yield g
 where
  pauseDuration :: Game -> Int
  pauseDuration g@Game {..} | _street == PreDeal          = 250000
                            | -- 0.25 second
                              _street == Showdown         = 4 * 1000000
                            | -- 4 seconds
                              countPlayersNotAllIn g <= 1 = 4 * 1000000
                            | otherwise                   = 1 * 1000000 -- 1 seconds


-- Progresses to the next state which awaits a player action.
--
--- If the next game state is one where no player action is possible
--  then we need to recursively progress the game.

--  These such states are:
--
--  1. everyone is all in.
--  1. All but one player has folded or the game.
--  3. Game is in the Showdown stage.
--
-- After each progression the new game state is sent to the table
-- mailbox. This sends the new game state through the pipeline that
-- the previous game state just went through.
progress :: GameEnv -> Consumer Game IO ()
progress GameEnv {..} = do
  g <- await
  liftIO $ print "can progress game in pipe?"
  liftIO $ print $ (canProgressGame g)
  when (canProgressGame g) (progress' g)
 where
  progress' game = do
    gen <- liftIO getStdGen
    liftIO $ setStdGen $ snd $ next gen
    liftIO $ print "PIPE PROGRESSING GAME"
    runEffect $ yield (progressGame gen game) >-> toOutput _envGameInMailbox


writeGameToDB :: GameEnv -> Pipe Game Game IO ()
writeGameToDB GameEnv {..} = do
  t <- liftIO $ dbGetTableEntity _envConnStr _envTableName
  let (Entity tableKey _) = fromMaybe notFoundErr t
  game <- await
  _    <- liftIO $ async $ dbInsertGame _envConnStr tableKey game
  yield game
 where
  notFoundErr =
    error $ "Table " <> show _envTableName <> " doesn't exist in DB"

-- write MsgOuts for new game states to outgoing mailbox for
-- client's who are observing the table
-- ensure they only get to see data they are allowed to see
informSubscriber :: TableName -> Game -> Client -> IO ()
informSubscriber n g Client {..} = do
  let filteredGame = excludeOtherPlayerCards clientUsername g
  runEffect $ yield (NewGameState n filteredGame) >-> toOutput outgoingMailbox
  return ()

-- sends new game states to subscribers
-- At the moment all clients receive updates from every game indiscriminately
broadcast :: GameEnv -> Pipe Game Game IO ()
broadcast GameEnv {..} = do
  game             <- await
  ServerState {..} <- liftIO $ readTVarIO _envServerState
  let usernames' = M.keys clients -- usernames to broadcast to
  liftIO $ async $ mapM_ (informSubscriber _envTableName game) clients
  yield game

logGame :: GameEnv -> Pipe Game Game IO ()
logGame GameEnv {..} = do
  game <- await
  yield game

-- Lookups up a table with the given name and writes the new game state
-- to the gameIn mailbox for propagation to observers.
--
-- If table with tableName is not found in the serverState lobby
-- then we just return () and do nothing.
toGameInMailbox :: TVar ServerState -> TableName -> Game -> IO ()
toGameInMailbox s name game = do
  table' <- atomically $ getTable s name
  forM_ table' send
  where send Table {..} = runEffect $ yield game >-> toOutput gameInMailbox


-- Get a combined outgoing mailbox for a group of clients who are observing a table
--
-- Here we monoidally combined so we then have one mailbox
-- we use to broadcast new game states to which will be sent out to each client's
-- socket connection under the hood
combineOutMailboxes :: [Client] -> Consumer MsgOut IO ()
combineOutMailboxes clients = toOutput $ foldMap outgoingMailbox clients

getTable :: TVar ServerState -> TableName -> STM (Maybe Table)
getTable s tableName = do
  ServerState {..} <- readTVar s
  return $ M.lookup tableName $ unLobby lobby

updateTable :: GameEnv -> Pipe Game Game IO ()
updateTable GameEnv {..} = do
  game <- await
  liftIO $ atomically $ updateTable' _envServerState _envTableName game
  yield game

updateTable' :: TVar ServerState -> TableName -> Game -> STM ()
updateTable' serverStateTVar tableName newGame = do
  ServerState {..} <- readTVar serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing               -> throwSTM $ TableDoesNotExistInLobby tableName
    Just table@Table {..} -> do
      let updatedLobby = updateTableGame tableName newGame lobby
      swapTVar serverStateTVar ServerState { lobby = updatedLobby, .. }
      return ()

updateTableAndGetMailbox
  :: TVar ServerState -> TableName -> Game -> STM (Maybe (Output Game))
updateTableAndGetMailbox serverStateTVar tableName newGame = do
  ServerState {..} <- readTVar serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing               -> throwSTM $ TableDoesNotExistInLobby tableName
    Just table@Table {..} -> do
      let updatedLobby = updateTableGame tableName newGame lobby
      swapTVar serverStateTVar ServerState { lobby = updatedLobby, .. }
      return $ Just gameInMailbox

updateTableGame :: TableName -> Game -> Lobby -> Lobby
updateTableGame tableName newGame (Lobby lobby) = Lobby
  $ M.adjust updateTable tableName lobby
  where updateTable Table {..} = Table { game = newGame, .. }
