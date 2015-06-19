import Control.LWC.Conc
import Shell
import Config

main :: IO ()
main = startSystem $ runShell defaultConfig
