import LwConc.Conc
import Shell
import Config
import LwConc.Threads

main :: IO ()
main = startSystem $ do
         tid <- myThreadId
         _ <- runShell (defaultConfig tid)
         return ()
