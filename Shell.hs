-- Shell of GHC
-- Implementation of a task shell by utilizing LWC
module Shell where
import Def
import Eval
import System.Console.Haskeline
import Control.Monad.IO.Class

startLoop = runInputT defaultSettings $ loop defaultConfig

loop :: ShellConfig -> InputT IO ()
loop config = do
  maybeInput <- getInputLine $ _prompt config
  case maybeInput of
    Nothing     -> loop config
    Just ""     -> loop config
    Just "quit" -> return ()
    Just input  -> do let ((mFeedback, mTransaction), config') = runShell (evaluate input cont) config
                      withJust mFeedback $ \fb -> outputStrLn fb
                      withJust mTransaction $ \io -> liftIO io
                      loop config'
  where
    cont :: IO ()
    cont = runInputT defaultSettings $ loop config
