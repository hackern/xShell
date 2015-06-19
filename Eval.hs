module Eval where
import Def

evaluate :: String -> Shell (Maybe String, IO ())
evaluate input = return (Just $ input ++ " " ++ input, print "transaction")
