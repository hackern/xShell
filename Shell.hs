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
    ShellConfig_ p _ io mio <- get
    put $ ShellConfig_ p reg io mio

  liftIO io = do
    ShellConfig_ p reg ios mio <- get
    put $ ShellConfig_ p reg (ios >> io) mio

  updateWithThread tio = do
    ShellConfig_ p reg iso _ <- get
    put $ ShellConfig_ p reg iso $ Just tio

runShell config = runInputT defaultSettings $ loop config

loop :: ShellConfig -> InputT IO ()
loop config = do
  maybeInput <- getInputLine $ _prompt config
  case maybeInput of
    Nothing     -> loop config
    Just ""     -> loop config
    Just "quit" -> return ()
    Just input  -> do let ((mFeedback, mCallBack), config') = runState (evaluate input) config
                      let (ShellConfig_ p reg _ _) = config'
                      withJust mFeedback $ \fb -> outputStrLn fb
                      case mCallBack of
                        Just cb -> do let Just io = _ioWithThread config'
                                      thread <- IO.liftIO io
                                      let (_, config'') = runState (cb thread) config'
                                      loop config''
                        Nothing -> do IO.liftIO $ _io config'
                                      loop (ShellConfig_ p reg (return ()) Nothing)
