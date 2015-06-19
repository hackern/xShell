module Eval where
import Data.Map as M
import Transaction
import Control.LWC.Conc

evaluate :: Transaction m => String -> m (Maybe String)
evaluate input = do
  case words input of
    ("run":name:_) -> do reg <- getRegistry
                         case M.lookup name reg of
                           Just (io, _) -> do runNow io
                                              return Nothing
                           Nothing      -> return $ Just "no such program"
                                   
{-    ("ps":_)       -> return $ (Nothing, Just $ dumpAllThreads print) -}
{-    ("bg":name:_)  -> do
      let (thread, io) = fg name cont
      reg <- getRegistry
      updateRegistry $ updateThread reg name thread      
      return (Nothing, Just io') -}
    _              -> return $ Just "no good thing to do!"
