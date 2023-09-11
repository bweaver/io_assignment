module Main (main) where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad -- hiding (forever)


import HDT.Tasks
import HDT.Interpreters hiding (ping, pong) 



-- THIS ROUTINE COMPILES, BUT DOES NOT WORK. 

-- FOR DEMONSTRATION OF THREADS, 
   -- run MyPingPong.hs with this commands:
      -- stack ghci     -- in directory containing MyPingPong.hs
      -- main
        
main :: IO ()
main = runIO [ping, pong]




