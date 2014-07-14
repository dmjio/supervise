module Concurrency.Supervise where

import           Control.Concurrent.Async  (Async (..), async, poll)
import           Control.Concurrent.MVar   (MVar, newMVar)
import           Control.Exception         (SomeException)
import           Control.Monad             (forM_, forever, replicateM)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (evalStateT, get, modify)
import           Data.List                 (delete)

type ThreadCount = Int
type RespawnTime = Int

data Supervise = Supervise {
      threadCount :: MVar Int -- * Thread count, must be greater than 0
    , respawnTime :: MVar Int -- * Respawn time, 0 or greater
    , adminThread :: Async () -- * Respawn time, 0 or greater
}

supervise ::
     ThreadCount -- * threads running concurrently
  -> RespawnTime  -- * respawn time
  -> IO a -- * action
  -> (SomeException -> IO b) -- * exception handler
  -> (a -> IO c)  -- * successful completion handler
  -> IO Supervise
supervise count respawn action exceptionHandler completionHandler = do
  threadCountMVar <- newMVar count :: IO (MVar Int)
  respawnMVar <- newMVar respawn :: IO (MVar Int)
  asyncs <- replicateM respawn $ async (forever action)
  admin <- async $ flip evalStateT asyncs $ forever $ do
    as <- get
    forM_ as $ \a -> do
        result <- liftIO $ poll a
        case result of
          Nothing -> return () -- not done yet
          Just (Right x) ->
              do liftIO $ completionHandler x
                 modify (delete a)
          Just (Left e) ->
              do liftIO $ exceptionHandler e
                 modify (delete a)
  return $ Supervise threadCountMVar respawnMVar admin









