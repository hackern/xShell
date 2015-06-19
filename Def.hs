module Def where
import Control.Monad.State

data ShellConfig = ShellConfig_ {
  _prompt :: String
}

type Shell a = State ShellConfig a -- Context Wrapper

defaultConfig = ShellConfig_ "> "

runShell = runState

withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust mx f = case mx of
  Just x  -> f x
  Nothing -> return ()
