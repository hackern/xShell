module Config where
import Control.LWC.Threads
import Apps.Printer
import Apps.Shower
import Apps.Counter
import Data.Map as M

type Registry = M.Map String (IO (), Maybe Thread)

initRegistry :: Registry
initRegistry = M.fromList [
  ("counter", (counter, Nothing))
  ]

data ShellConfig = ShellConfig_ {
  _prompt   :: String,
  _registry :: Registry,
  _ios      :: [IO ()]
}

defaultConfig = ShellConfig_ {
  _prompt   = "> ",
  _registry = initRegistry,
  _ios      = []
}

