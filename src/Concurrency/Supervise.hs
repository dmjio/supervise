module Concurrency.Supervise
    ( ThreadCount
    , RespawnTime
    , Supervise (..)
    , ThreadCount (..)
    , RespawnTime (..)
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

newtype ThreadCount = ThreadCount Int
newtype RespawnTime = RespawnTime Int

data Supervise = Supervise {
      threadCount :: MVar ThreadCount -- * Thread count, must be greater than 0
    , respawnTime :: MVar RespawnTime -- * Respawn time, 0 or greater
    , adminThread :: Async ()         -- * Admin thread for inspecting and monitoring
  }

secs :: Int -> Int
secs = (*1000000)

supervise ::
     ThreadCount             -- * Threads running concurrently
  -> RespawnTime             -- * Respawn time
  -> IO a                    -- * action
  -> (SomeException -> IO b) -- * exception handler
  -> (a -> IO c)             -- * successful completion handler
  -> IO Supervise
supervise tc@(ThreadCount count) rt@(RespawnTime respawnTime) 
             action exceptionHandler completionHandler = do
  threadCountMVar <- newMVar tc :: IO (MVar ThreadCount)
  respawnMVar     <- newMVar rt :: IO (MVar RespawnTime)
  asyncs          <- replicateM count $ async action
  adminThread     <- async $ flip evalStateT asyncs $ forever $ do
                       as <- get
                       forM_ as $ \a -> do
                         result <- liftIO $ poll a
                         case result of
                           Nothing -> return ()
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
           MVar ThreadCount -> 
           MVar RespawnTime -> 
           IO a -> 
           StateT [Async a] m ()
respawn threadCountMVar respawnMVar action = do
          (tc@(ThreadCount threadCount), rt@(RespawnTime respawnTime)) <- 
              liftIO $ (,) <$> takeMVar threadCountMVar
                           <*> takeMVar respawnMVar
          numAsyncs <- length <$> get
          newActions <- liftIO $ 
                          case threadCount - numAsyncs of
                            x | x > 0 -> replicateM x $ async $ 
                                         do threadDelay $ secs respawnTime
                                            action
                              | otherwise -> return []
          modify (++newActions)
          liftIO $ do putMVar threadCountMVar tc
                      putMVar respawnMVar rt









