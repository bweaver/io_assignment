import Control.Concurrent.STM
import Control.Concurrent
import System.Random
import Control.Monad -- hiding (forever)

 -- HACK with runPingAndPong-

data PingPong = Ping |Pong deriving Show

type SharedChannel = TChan (Int, PingPong)
type LoggingQueue  = TChan String

main = runPingAndPong [ping, pong]

runIO :: [SharedChannel -> LoggingQueue -> Int -> IO ()] -> IO ()
runIO list  = do  

          sharedChan <- atomically $ newTChan
          loggingQueue <- loggingQueue


  --        let list = [ping, pong]
          let zList = zip [1..(length list + 1)] list
          forM_ zList (\(n, task) -> (item (task sharedChan loggingQueue)  n)) 
          
    where 
    item the_task id = forkIO ((do { (the_task id )}))




runPingAndPong :: [SharedChannel -> LoggingQueue -> Int -> IO ()] -> IO ()
runPingAndPong list  = do  

          sharedChan <- atomically $ newTChan
          loggingQueue <- loggingQueue


  --        let list = [ping, pong]
          let zList = zip [1..(length list + 1)] list
          forM_ zList (\(n, task) -> (item (task sharedChan loggingQueue)  n)) 
          
    where 
    item the_task id = forkIO ((do { (the_task id )}))





loggingQueue :: IO(LoggingQueue)
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



sequentialLog :: PingPong -> LoggingQueue -> IO ()
sequentialLog pp loggingQueue = atomically $ writeTChan loggingQueue $ show pp  


receive :: SharedChannel -> STM (Int, PingPong)
receive sharedC = do 
  status <- tryReadTChan sharedC
  case status of
    Nothing -> retry
    Just (id, msg) -> return (id, msg) 


ping :: SharedChannel -> LoggingQueue -> Int -> IO ()
ping sharedC loggingQueue idOfThread = delay >> broadcast (idOfThread, Ping) sharedC >> sequentialLog Ping loggingQueue >> go
  where go = do 
           (id, msg) <- atomically $ receive sharedC 
           case msg of 
             Ping -> (atomically $ unGetTChan sharedC (idOfThread, Ping)) >> go
             Pong -> 
               ping sharedC loggingQueue idOfThread 

delay :: IO ()
delay = threadDelay (10^6)

broadcast :: (Int, PingPong) -> SharedChannel -> IO ()
broadcast (id, msg) sharedC = 
  atomically $ writeTChan sharedC (id, msg) 

pong :: SharedChannel -> LoggingQueue -> Int -> IO ()
pong sharedC loggingQueue idOfThread =  do 
         (_, msg) <- atomically $ receive sharedC 
         case msg of   
           Ping ->  
             delay >> broadcast (idOfThread, Pong) sharedC >> sequentialLog Pong loggingQueue >> pong sharedC loggingQueue idOfThread 
                      
           Pong ->  (
             (atomically $ unGetTChan sharedC (idOfThread, Pong)) >> pong sharedC loggingQueue idOfThread )
