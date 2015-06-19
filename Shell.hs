{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Shell (
  Shell,
  runShell
) where

import Transaction
import Config
import Eval
import Monad
import System.Console.Haskeline
import Control.Monad.IO.Class as IO
import Control.Monad.State

type Shell = State ShellConfig

instance Transaction Shell where
  getCont = do
   config <- get
   return $ runShell config

  getRegistry = gets _registry

  updateRegistry reg = do
    ShellConfig_ p _ io <- get
    put $ ShellConfig_ p reg io

  liftIO io = do
    ShellConfig_ p reg ios <- get
    put $ ShellConfig_ p reg (ios >> io)

runShell config = runInputT defaultSettings $ loop config

loop :: ShellConfig -> InputT IO ()
loop config = do
  maybeInput <- getInputLine $ _prompt config
  case maybeInput of
    Nothing     -> loop config
    Just ""     -> loop config
    Just "quit" -> return ()
    Just input  -> do let (mFeedback, config') = runState (evaluate input) config
                      let (ShellConfig_ p reg io) = config'
                      IO.liftIO io
                      withJust mFeedback $ \fb -> outputStrLn fb
                      loop (ShellConfig_ p reg $ return ())
