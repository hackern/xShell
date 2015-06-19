module Apps.Counter where
import Control.LWC.Conc
import Data.Time.Clock.POSIX
import Control.Monad

counter :: IO ()
counter = do
  time <- getPOSIXTime
  forever $ do
    now <- getPOSIXTime
    print $ now - time
