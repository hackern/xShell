module Transaction (
  Transaction(..)
, updateThread
, runNow  
) where
import Data.Map as M
import Config
-- import Control.Monad.IO.Class
import Control.LWC.Conc
import Control.LWC.Threads
import Control.LWC.MVar

class Monad m => Transaction m where
  liftIO  :: IO () -> m ()
  getCont :: m (IO ())
  getRegistry :: m Registry
  updateRegistry :: Registry -> m ()

updateThread :: Transaction m => String -> Thread -> m ()
updateThread name thread = do
  reg <- getRegistry
  let Just (io, _) = M.lookup name reg
  updateRegistry $ M.insert name (io, Just thread) reg

runNow :: Transaction m => IO () -> m ()
runNow io = do
  cont <- getCont
  liftIO $ do
    tid <- newThreadId
    sc <- newSCont $ io >> cont
    switchT (\_ -> return $ Thread tid sc)
