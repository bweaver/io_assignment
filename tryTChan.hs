module Main (main) where

import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TChan
import GHC.Conc
import System.Random

readQ = atomically . readTChan
writeQ ch v = atomically $ writeTChan ch v

main2 :: IO ()
main2 = do
    queue <- atomically $ newTChan
    readerThread queue
    loop queue 0
    where loop queue v = (getStdRandom $ randomR (100000,10000000)) >>= threadDelay >> writeQ queue v >> loop queue (v+1)

readerThread :: TChan Integer -> IO ThreadId
readerThread queue = forkIO loop
    where loop = readQ queue >>= putStrLn . (\s -> "Received: " ++ s) . show >> loop


main :: IO () 
main = do
    let users = [1, 2]
    sharedChan <- atomically $ newTChan
    forM_ users (\user -> createAndRunThread user sharedChan) 
type User = Int 



createAndRunThread :: User -> TChan (ThreadId, Int) -> IO ()
createAndRunThread user sharedC = loop
  where loop = do
          threadId <- forkIO $ putStrLn "hw"
          threadDelay (10^6)
          atomically $ writeTChan sharedC (threadId, user)
          (atomically . readTChan) sharedC  >>= (\(u, id) -> putStrLn $ (show id) ++ "  " ++ (show u)) >> return ()
          loop
