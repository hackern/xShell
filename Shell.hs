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
import Control.Monad.IO.Class
import Control.Monad.State

type Shell = State ShellConfig

instance Transaction Shell where
  getCont = do
   config <- get
   return $ runShell config

  getRegistry = gets _registry

  updateRegistry reg = do
    ShellConfig_ p _ ios <- get
    put $ ShellConfig_ p reg ios

  liftIO io = do
    ios <- gets _ios
    updateIOs (io : ios)

updateIOs ios = do
  ShellConfig_ p reg _ <- get
  put $ ShellConfig_ p reg ios

runShell config = runInputT defaultSettings $ loop config

loop :: ShellConfig -> InputT IO ()
loop config = do
  maybeInput <- getInputLine $ _prompt config
  case maybeInput of
    Nothing     -> loop config
    Just ""     -> loop config
    Just "quit" -> return ()
    Just input  -> do let ((mFeedback, mOperation), config') = runState (evaluate input) config
                      withJust mFeedback $ \fb -> outputStrLn fb
                      loop config'
