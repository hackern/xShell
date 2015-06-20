{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Shell (
  Shell,
  runShell
) where

import Transaction
import Config
import Eval
import Monad
import Control.Monad.State
import LwConc.Conc
import System.IO
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
    put $ ShellConfig_ p reg iso (Just tio)

runShell :: ShellConfig -> IO ()
runShell config = do
  putStr $ _prompt config
  hFlush stdout
  input <- getLine
  case input of
    ""     -> return ()
    "quit" -> runShell config
    input  -> do let ((mFeedback, mCallBack), config') = runState (evaluate input) config
                 let (ShellConfig_ p reg _ _) = config'
                 withJust mFeedback $ \fb -> print fb
                 case mCallBack of
                   Just cb -> do let Just io = _ioWithThread config'
                                 thread <- io
                                 let (_, config'') = runState (cb thread) config'
                                 runShell config''
                   Nothing -> do _io config'
                                 runShell (ShellConfig_ p reg (return ()) Nothing)
