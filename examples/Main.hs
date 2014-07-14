module Main where

import Concurrency.Supervise
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Exit
import System.Random

secs :: Int -> Int
secs = (*1000000)

main :: IO ()
main = do
  Supervise threadcount respawnTime adminThread <- 
      supervise 
        (ThreadCount 5)   -- This means 5 threads will run 'action' concurrently 
        (RespawnTime 0)   -- Time elapsed before the next thread respawns
        action            -- IO action to run
        handleExceptions  -- Exception handler
        handleCompletions -- Completion handler

  -- Monitor the admin thread
  forever $ do
         res <- poll adminThread
         case res of
           Nothing -> do putStrLn "adming running..." 
                         threadDelay (secs 3)
           Just (Right x) -> putStrLn "admin completed.."
           Just (Left x) -> putStrLn "admin error.."
         threadDelay (secs 2)

  where handleExceptions    = print
        handleCompletions x = print "finished action.."
        action              = do r <- randomRIO (1,10)
                                 threadDelay (secs r)
                                 putStrLn $ "running action..." ++ show r ++ " secs"

  
