module Concurrent where
import Control.LWC.MVar
import Control.LWC.Threads
import LwConc.PTM
import System.IO.Unsafe(unsafePerformIO)

mainThread :: MVar ThreadId
mainThread = unsafePerformIO $ newEmptyMVar

updateMainThread :: ThreadId -> IO ()
updateMainThread tid = putMVar mainThread tid

output :: Show a => a -> IO ()
output a = do
  withMVar mainThread $ \mainId -> do
    Just myId <- atomically $ mySafeThreadId
    if myId == mainId then
       print a
    else return ()
