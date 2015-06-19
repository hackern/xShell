module Commit where
import Control.LWC.Conc
import Control.LWC.Threads
import Control.LWC.MVar
import Data.Map as M
-- import Control.Monad.IO.Class
import Transaction

data Operation = UpdateThread String Thread
               | RunNow (IO ())

commit :: Transaction m => Operation -> m ()

commit (UpdateThread name thread) = do
  reg <- getRegistry
  let Just (io, _) = M.lookup name reg
  updateRegistry $ M.insert name (io, Just thread) reg

commit (RunNow io) = do
  cont <- getCont
  liftIO $ do
    tid <- newThreadId
    sc <- newSCont $ io >> cont
    switchT (\_ -> return $ Thread tid sc)
