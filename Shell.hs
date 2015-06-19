-- Shell of GHC
-- Implementation of a task shell by utilizing LWC
import Def
import Eval
import System.Console.Haskeline
import Control.Monad.IO.Class

main :: IO ()
main = runInputT defaultSettings $ loop defaultConfig

loop :: ShellConfig -> InputT IO ()
loop config = do
  maybeInput <- getInputLine $ _prompt config
  case maybeInput of
    Nothing     -> loop config
    Just ""     -> loop config
    Just "quit" -> return ()
    Just input  -> do let ((mFeedback, transaction), config') = runShell (evaluate input) config
                      case mFeedback of
                        Just feedback -> outputStrLn feedback
                      liftIO transaction
                      loop config'

