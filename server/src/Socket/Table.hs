{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Socket.Table where
import           Control.Concurrent      hiding ( yield )
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Data.Map.Lazy                 as M
import           Database.Persist.Postgresql
import qualified Network.WebSockets            as WS

import           Database.Persist.Postgresql    ( ConnectionString )
import           Types
import           Control.Lens            hiding ( Fold )
import           Poker.Types             hiding ( LeaveSeat )

import           Control.Monad.Except
import           Control.Monad.Reader

import           System.Random
import           Poker.Game.Blinds
import           Poker.Game.Privacy
import           Poker.Game.Game
import           Poker.Types                    ( Player )
import           Data.Maybe
import           Poker.Game.Utils
import           Socket.Types
import           Socket.Utils
import           Poker.Poker

import           Database

import           Pipes.Aeson
import           Pipes                   hiding ( next )
import           Pipes.Core                     ( push )
import           Pipes.Concurrent
import           Pipes.Lift
import           Pipes.Parse             hiding ( decode
                                                , encode
                                                , next
                                                )
import qualified Pipes.Prelude                 as P

import           Prelude


newtype AICount = AICount Int deriving (Show, Eq)

data GameEnv = GameEnv
  { _envConnStr :: ConnectionString
  , _envServerState :: TVar ServerState
  , _envTableName :: TableName
  , _envGameOutMailbox :: Input Game
  , _envGameInMailbox:: Output Game
  , _envAICount :: AICount
  }

makeLenses ''GameEnv

type GamePipe a = Pipe Game Game (ReaderT GameEnv IO) a


-- New game states are send to the table's incoming mailbox every time a player acts
-- in a way that follows the game rules. These new game states are then processed
-- in our game pipeline.
setUpTablePipes
  :: ConnectionString -> TVar ServerState -> TableName -> Table -> IO (Async ())
setUpTablePipes _envConnStr _envServerState _envTableName Table {..} = do
  async
    $   forever
    $   runEffect
    $   runReaderP env
    $   gameProducer
    >-> gamePipeline
    >-> progress
 where
  env = GameEnv
    { _envGameInMailbox  = gameInMailbox
    , _envGameOutMailbox = gameOutMailbox
    , _envAICount        = AICount 2
    , ..
    }
  gameProducer = fromInput gameOutMailbox


-- this is the pipeline of effects we run everytime a new game state
-- is placed in the tables
-- incoming mailbox for new game states.
gamePipeline :: Pipe Game Game (ReaderT GameEnv IO) ()
gamePipeline = do
  broadcast
    >-> logGame
    >-> updateTable
    >-> writeGameToDB
    >-> nextStagePause
    >-> timePlayer


timePlayer :: Pipe Game Game (ReaderT GameEnv IO) ()
timePlayer = do
  GameEnv {..} <- ask
  g@Game {..}  <- await
  let currPlyrToAct = (!!) (getGamePlayerNames g) <$> _currentPosToAct
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


-- Delay between game stages so users don't just see a quick flurry of game states
nextStagePause :: Pipe Game Game (ReaderT GameEnv IO) ()
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
progress :: Consumer Game (ReaderT GameEnv IO) ()
progress = do
  GameEnv {..} <- ask
  g            <- await
  liftIO $ print "can progress game in pipe?"
  liftIO $ print $ (canProgressGame g)
  when (canProgressGame g) (progress' g _envGameInMailbox)
 where
  progress' game gInMailbox = do
    gen <- liftIO getStdGen
    liftIO $ setStdGen $ snd $ next gen
    liftIO $ print "PIPE PROGRESSING GAME"
    runEffect $ yield (progressGame gen game) >-> toOutput gInMailbox


writeGameToDB :: Pipe Game Game (ReaderT GameEnv IO) ()
writeGameToDB = do
  GameEnv {..} <- ask
  table        <- liftIO $ dbGetTableEntity _envConnStr _envTableName
  let (Entity tableKey _) = fromMaybe (notFoundErr _envTableName) table
  game <- await
  _    <- liftIO $ async $ dbInsertGame _envConnStr tableKey game
  yield game
 where
  notFoundErr name = error $ "Table " <> show name <> " doesn't exist in DB"

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
broadcast :: Pipe Game Game (ReaderT GameEnv IO) ()
broadcast = do
  GameEnv {..}     <- ask
  game             <- await
  ServerState {..} <- liftIO $ readTVarIO _envServerState
  let usernames' = M.keys clients -- usernames to broadcast to
  liftIO $ async $ mapM_ (informSubscriber _envTableName game) clients
  yield game

logGame :: Pipe Game Game (ReaderT GameEnv IO) ()
logGame = do
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

updateTable :: Pipe Game Game (ReaderT GameEnv IO) ()
updateTable = do
  GameEnv {..} <- ask
  game         <- await
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
