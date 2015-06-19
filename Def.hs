module Def where
import Control.Monad.State

data ShellConfig = ShellConfig_ {
  _prompt :: String
}

type Shell a = State ShellConfig a -- Context Wrapper

defaultConfig = ShellConfig_ "> "

runShell = runState
