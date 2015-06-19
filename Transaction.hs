module Transaction (
  Transaction(..)
, runNow
, runBack
, updateThread
) where
import Data.Map as M
import Config
import Control.LWC.Conc
import Control.LWC.Threads
import Control.LWC.MVar

class Monad m => Transaction m where
  liftIO :: IO () -> m ()
  getCont :: m (IO ())
  getRegistry :: m Registry
  updateRegistry :: Registry -> m ()
  updateWithThread :: (IO Thread) -> m ()

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

runBack :: Transaction m => IO () -> String -> m (Thread -> m ())
runBack io name = do
  updateWithThread (fork io)
  return (\thread -> updateThread name thread)
  
