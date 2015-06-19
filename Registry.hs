module Registry where
import Control.LWC.Conc
import Data.Map as M
import Printer
import Shower

registry :: M.Map String (IO ())
registry = M.fromList [("printer", printer), ("shower", shower)]

exec :: String -> (Maybe String, Maybe (IO ()))
exec name = case M.lookup name registry of 
    Nothing      -> (Just "no such program", Nothing)
    Just program -> (Nothing, Just $ launch program)
    where
      launch :: IO () -> IO ()
      launch io = do
        _ <- forkIO io
        return ()

