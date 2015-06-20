module Apps.Counter where
import Data.Time.Clock.POSIX
import LwConc.Conc
import Control.Monad

counter :: IO ()
counter = do
  time <- getPOSIXTime
  forever $ do
    now <- getPOSIXTime
    print $ now - time
    threadDelay 100
