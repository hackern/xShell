module Eval where
import Data.Map as M
import Transaction
import Control.LWC.Conc
import Commit

evaluate :: Transaction m => String -> m (Maybe String, Maybe Operation)
evaluate input = do
  case words input of
    ("run":name:_) -> do reg <- getRegistry
                         case M.lookup name reg of
                           Just (io, _) -> return (Nothing, Just $ RunNow io)
                           Nothing      -> return (Just "no such program", Nothing)
                                   
{-    ("ps":_)       -> return $ (Nothing, Just $ dumpAllThreads print) -}
{-    ("bg":name:_)  -> do
      let (thread, io) = fg name cont
      reg <- getRegistry
      updateRegistry $ updateThread reg name thread      
      return (Nothing, Just io') -}
    _              -> return (Just "no good thing to do!", Nothing)
