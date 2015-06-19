module Monad where


withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust mx f = case mx of
  Just x  -> f x
  Nothing -> return ()
