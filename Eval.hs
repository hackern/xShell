module Eval where
import Data.Map as M
import Transaction
import LwConc.Threads
import LwConc.Conc
import Config

evaluate :: Transaction m => String -> m (Maybe String, Maybe (Thread -> m ()))
evaluate input = do
  case words input of
    -- Run at foreground
    ("run":name:_) -> do fb <- withNameSimple name (\io -> runNow io)
                         return (fb, Nothing)
    -- Show current running threads
    ("ps":_)       -> do liftIO $ dumpAllThreads print
                         return (Nothing, Nothing)
    -- Run at background
    ("bg":name:_)  -> do mcb <- withNameCb name (\io -> do runBack io name)
                         case mcb of
                           Just cb -> return (Nothing, Just cb)
                           Nothing -> return (Just "no such program", Nothing)
    ("fg":name:_)  -> do withNameThread name switchThread
                         return (Nothing, Nothing)
    _              -> return (Just "no good thing to do!", Nothing)


withNameSimple :: Transaction m => String -> (IO () -> m ()) -> m (Maybe String)
withNameSimple name f = do
  reg <- getRegistry
  case M.lookup name reg of
    Just (io, _) -> f io >> return Nothing
    Nothing      -> return $ Just "no such program"

withNameCb :: Transaction m => String -> (IO () -> m (Thread -> m ())) -> m (Maybe (Thread -> m ()))
withNameCb name f = do
  reg <- getRegistry
  case M.lookup name reg of
    Just (io, _)  -> do cb <- f io
                        return $ Just cb
    Nothing       -> return Nothing

withNameThread name f = do
  reg <- getRegistry
  case M.lookup name reg of
    Just (_, Just t)   -> f t
    Nothing       -> return ()
