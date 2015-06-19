module Eval where
import Def
import Registry

evaluate :: String -> IO () -> Shell (Maybe String, Maybe (IO ()))
evaluate input cont = do
  case words input of
    ("run":name:_) -> return $ exec name cont
    _              -> return (Just "no good thing to do!", Nothing)
