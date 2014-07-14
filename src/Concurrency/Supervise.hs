module Concurrency.Supervise
    ( ThreadCount
    , RespawnTime
    , Supervise (..)
    , supervise
      -- * re-exports for your convenience
    , poll
    , Async(..)
    )
    where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async  (Async (..), async, poll)
import           Control.Concurrent.MVar   (MVar, newMVar, putMVar, takeMVar)
import           Control.Exception         (SomeException)
import           Control.Monad             (forM_, forever, replicateM)
import           Control.Monad.IO.Class    (liftIO, MonadIO)
import           Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import           Data.List                 (delete)

type ThreadCount = Int
type RespawnTime = Int

data Supervise = Supervise {
      threadCount :: MVar Int -- * Thread count, must be greater than 0
    , respawnTime :: MVar Int -- * Respawn time, 0 or greater
    , adminThread :: Async () -- * Respawn time, 0 or greater
}

secs :: Int -> Int
secs = (*1000000)

supervise ::
     ThreadCount -- * threads running concurrently
  -> RespawnTime  -- * respawn time
  -> IO a -- * action
  -> (SomeException -> IO b) -- * exception handler
  -> (a -> IO c)  -- * successful completion handler
  -> IO Supervise
supervise count respawnTime action exceptionHandler completionHandler = do
  threadCountMVar <- newMVar count :: IO (MVar ThreadCount)
  respawnMVar <- newMVar respawnTime :: IO (MVar RespawnTime)
  asyncs <- replicateM count $ async (forever action)
  adminThread <- async $ flip evalStateT asyncs $ forever $ do
    as <- get
    forM_ as $ \a -> do
        result <- liftIO $ poll a
        case result of
          Nothing -> return () -- not done yet
          Just (Right x) -> -- completed with no errors
              do liftIO $ completionHandler x
                 modify (delete a)
                 respawn threadCountMVar respawnMVar action
          Just (Left e) -> -- completed with errors
              do liftIO $ exceptionHandler e
                 modify (delete a)
                 respawn threadCountMVar respawnMVar action
  return $ Supervise threadCountMVar respawnMVar adminThread

respawn :: (MonadIO m, Functor m) =>
           MVar Int -> 
           MVar Int -> 
           b -> 
           StateT [Async a] m ()
respawn threadCountMVar respawnMVar action = do
          (threadCount, respawnTime) <- 
              liftIO $ (,) <$> takeMVar threadCountMVar
                           <*> takeMVar respawnMVar
          numAsyncs <- length <$> get
          newActions <- case numAsyncs - threadCount of
                 x | x > 0 -> liftIO $ replicateM x $ async $ forever $
                                do threadDelay $ secs respawnTime
                                   return [action]
                   | otherwise -> return []
          modify (++newActions)
          liftIO $ do putMVar threadCountMVar threadCount
                      putMVar respawnMVar respawnTime









