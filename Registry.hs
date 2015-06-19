module Registry where
import Control.LWC.Conc
import Control.LWC.Threads
import Data.Map as M
import Printer
import Shower

registry :: M.Map String (IO ())
registry = M.fromList [("printer", printer), ("shower", shower)]

exec :: String -> IO () -> (Maybe String, Maybe (IO ()))
exec name cont = case M.lookup name registry of 
    Nothing      -> (Just "no such program", Nothing)
    Just program -> (Nothing, Just $ goto program cont)

fork :: IO () -> IO ()
fork io = do
  _ <- forkIO io
  return ()

goto :: IO () -> IO () -> IO ()
goto io cont = do
  tid <- newThreadId
  sc <- newSCont $ do
        io
        cont
  switchT (\_ -> return $ Thread tid sc)
