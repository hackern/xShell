module Eval where
import Data.Map as M
import Transaction
import Control.LWC.Threads
import Control.LWC.Conc

evaluate :: Transaction m => String -> m (Maybe String, Maybe (Thread -> m ()))
evaluate input = do
  case words input of
    -- Run at foreground
    ("run":name:_) -> do fb <- withName name (\io -> runNow io)
                         return (fb, Nothing)
    -- Show current running threads
    ("ps":_)       -> do liftIO $ dumpAllThreads print
                         return (Nothing, Nothing)
    -- Run at background
    ("bg":name:_)  -> do fb <- withName name (\io -> do runBack io name)
                         return (fb, Just (\thread -> updateThread name thread))
    _              -> return (Just "no good thing to do!", Nothing)


withName :: Transaction m => String -> (IO () -> m a) -> m (Maybe String)
withName name f = do
  reg <- getRegistry
  case M.lookup name reg of
    Just (io, _) -> f io >> return Nothing
    Nothing      -> return $ Just "no such program"
