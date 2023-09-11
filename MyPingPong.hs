import Control.Concurrent.STM
import Control.Concurrent
import System.Random
import Control.Monad -- hiding (forever)

 -- HACK with runPingAndPong-

data PingPong = Ping |Pong deriving Show

main = runPingAndPong [ping, pong]

runIO :: [TChan (Int, PingPong) -> TChan String -> Int -> IO ()] -> IO ()
runIO list  = do  

          sharedChan <- atomically $ newTChan
          loggingQueue <- loggingQueue


  --        let list = [ping, pong]
          let zList = zip [1..(length list + 1)] list
          forM_ zList (\(n, task) -> (item (task sharedChan loggingQueue)  n)) 
          
    where 
    item the_task id = forkIO ((do { (the_task id )}))




runPingAndPong :: [TChan (Int, PingPong) -> TChan String -> Int -> IO ()] -> IO ()
runPingAndPong list  = do  

          sharedChan <- atomically $ newTChan
          loggingQueue <- loggingQueue


  --        let list = [ping, pong]
          let zList = zip [1..(length list + 1)] list
          forM_ zList (\(n, task) -> (item (task sharedChan loggingQueue)  n)) 
          
    where 
    item the_task id = forkIO ((do { (the_task id )}))





loggingQueue :: IO(TChan String)
loggingQueue = do
    queue <- newTChanIO
    forkIO . forever $ atomically (readTChan queue) >>= putStrLn
    return queue

sharedChan :: IO (TChan PingPong)
sharedChan = atomically newTChan


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



sequentialLog :: PingPong -> TChan String -> IO ()
sequentialLog pp loggingQueue = atomically $ writeTChan loggingQueue $ show pp  


receive :: TChan (Int, PingPong) -> STM (Int, PingPong)
receive sharedC = do 
  status <- tryReadTChan sharedC
  case status of
    Nothing -> retry
    Just (id, msg) -> return (id, msg) 


ping :: TChan (Int, PingPong) -> TChan String -> Int -> IO ()
ping sharedC loggingQueue idOfThread = delay >> broadcast (idOfThread, Ping) sharedC >> sequentialLog Ping loggingQueue >> go
  where go = do 
           (id, msg) <- atomically $ receive sharedC 
           case msg of 
             Ping -> (atomically $ unGetTChan sharedC (idOfThread, Ping)) >> go
             Pong -> 
               ping sharedC loggingQueue idOfThread 

delay :: IO ()
delay = threadDelay (10^6)

broadcast :: (Int, PingPong) -> TChan (Int, PingPong) -> IO ()
broadcast (id, msg) sharedC = 
  atomically $ writeTChan sharedC (id, msg) 

pong :: TChan (Int, PingPong) -> TChan String -> Int -> IO ()
pong sharedC loggingQueue idOfThread =  do 
         (_, msg) <- atomically $ receive sharedC 
         case msg of   
           Ping ->  
             delay >> broadcast (idOfThread, Pong) sharedC >> sequentialLog Pong loggingQueue >> pong sharedC loggingQueue idOfThread 
                      
           Pong ->  (
             (atomically $ unGetTChan sharedC (idOfThread, Pong)) >> pong sharedC loggingQueue idOfThread )


randomDelay :: IO ()
-- Delay for a random time between 1 and 1000,000 microseconds
randomDelay = do { waitTime <- getStdRandom (randomR (1, 1000000))
                 ; threadDelay waitTime }
