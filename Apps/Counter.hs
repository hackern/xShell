module Apps.Counter where
import Control.LWC.Conc
import Data.Time.Clock.POSIX
import Control.Monad
import Concurrent

output = print

counter :: IO ()
counter = do
  time <- getPOSIXTime
  forever $ do
    now <- getPOSIXTime
    output $ now - time
    threadDelay 100
