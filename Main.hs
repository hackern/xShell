import Control.LWC.Conc
import Shell
import Config
import Control.LWC.Threads
import Concurrent

main :: IO ()
main = startSystem $ do
         tid <- myThreadId
         _ <- runShell (defaultConfig tid)
         return ()
