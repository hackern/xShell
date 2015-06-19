module Transaction where
import Config
import Control.Monad.IO.Class

class Monad m => Transaction m where
  liftIO  :: IO () -> m ()
  getCont :: m (IO ())
  getRegistry :: m Registry
  updateRegistry :: Registry -> m ()

