module Concurrent where
import Control.LWC.MVar
import Control.LWC.Threads
import LwConc.PTM

{-
output :: Show a => a -> IO ()
output a = do
  withMVar mainThread $ \mainId -> do
    Just myId <- atomically $ mySafeThreadId
    if myId == mainId then
       print a
    else return ()
-}
