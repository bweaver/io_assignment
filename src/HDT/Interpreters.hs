
module HDT.Interpreters
    ( 
      delay
    , broadcast  
    , receive 
    , ping
    , pong 
    , loggingQueue
    , sharedChan
    , runPingAndPong
    ) where 

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad -- hiding (forever)

{-
main = do  

          sharedChan <- atomically $ newTChan
          loggingQueue <- initializeLoggingQueue


          let list = [ping, pong]
              zList = zip [1..(length list + 1)] list
          forM_ zList (\(n, task) -> (item (task sharedChan loggingQueue)  n)) 
          
    where 
    item the_task id = forkIO ((do { (the_task id )}))

initializeLoggingQueue :: IO(TChan String)
initializeLoggingQueue = do
    queue <- newTChanIO
    forkIO . forever $ atomically (readTChan queue) >>= putStrLn
    return queue
-}


--ping = delay >> broadcast Ping >> loop
--  where
--    loop = do
--        msg <- receive
--        case msg of
--            Ping -> loop
--            Pong -> ping

--x delay 
--broadcast
--  broadcast
--  logs

--loop
--  receive 
--    block until message is received
--    loop
--    ping



loggingQueue :: IO(TChan String)
loggingQueue = do
    queue <- newTChanIO
    forkIO . forever $ atomically (readTChan queue) >>= putStrLn
    return queue

sharedChan :: IO (TChan String)
sharedChan = atomically newTChan

sequentialLog :: String -> TChan String -> IO ()
sequentialLog msg loggingQueue = atomically $ writeTChan loggingQueue msg  

runPingAndPong :: [TChan (Int, String) -> TChan String -> Int -> IO ()] -> IO ()
runPingAndPong list  = do  

          sharedChan <- atomically $ newTChan
          loggingQueue <- loggingQueue


  --        let list = [ping, pong]
          let zList = zip [1..(length list + 1)] list
          forM_ zList (\(n, task) -> (item (task sharedChan loggingQueue)  n)) 
          
    where 
    item the_task id = forkIO ((do { (the_task id )}))

receive :: TChan (Int, String) -> STM (Int, String)
receive sharedC = do 
  status <- tryReadTChan sharedC
  case status of
    Nothing -> retry
    Just (id, msg) -> return (id, msg) 


ping :: TChan (Int, String) -> TChan String -> Int -> IO ()
ping sharedC loggingQueue idOfThread = delay >> broadcast (idOfThread, "Ping") sharedC >> sequentialLog "Ping" loggingQueue >> go
  where go = do 
           (id, msg) <- atomically $ receive sharedC 
           case msg of 
             "Ping" -> (atomically $ unGetTChan sharedC (idOfThread, "Ping")) >> go
             "Pong" -> 
               ping sharedC loggingQueue idOfThread 

delay :: IO ()
delay = threadDelay (10^6)

broadcast :: (Int, String) -> TChan (Int, String) -> IO ()
broadcast (id, msg) sharedC = 
  atomically $ writeTChan sharedC (id, msg) 

pong :: TChan (Int, String) -> TChan String -> Int -> IO ()
pong sharedC loggingQueue idOfThread =  do 
         (_, msg) <- atomically $ receive sharedC 
         case msg of   
           "Ping" ->  
             delay >> broadcast (idOfThread, "Pong") sharedC >> sequentialLog "Pong" loggingQueue >> pong sharedC loggingQueue idOfThread 
                      
           "Pong" ->  (
             (atomically $ unGetTChan sharedC (idOfThread, "Pong")) >> pong sharedC loggingQueue idOfThread )