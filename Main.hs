import Control.LWC.Conc
import Shell
import Config
import Control.LWC.Threads
import Concurrent

main :: IO ()
main = startSystem $ do
         tid <- myThreadId
         updateMainThread tid
         _ <- runShell (defaultConfig tid)
         return ()
