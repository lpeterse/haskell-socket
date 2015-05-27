module System.Socket.Internal.Event
  ( threadWaitReadMVar, threadWaitWriteMVar, threadWaitWrite', threadWaitRead'
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

import Foreign.C.Error

import GHC.Conc (threadWaitReadSTM, threadWaitWriteSTM, atomically)

import System.Posix.Types ( Fd(..) )

import System.Socket.Internal.Socket

-------------------------------------------------------------------------------
-- Helpers for threadsafe event registration on file descriptors
-------------------------------------------------------------------------------

threadWaitReadMVar :: MVar Fd -> IO ()
threadWaitReadMVar mfd = do
  wait <- withMVar mfd $ \fd-> do
    when (fd < 0) $ throwIO (SocketException eBADF)
    threadWaitReadSTM fd >>= return . atomically . fst
  wait `onException` throwIO (SocketException eBADF)

threadWaitWriteMVar :: MVar Fd -> IO ()
threadWaitWriteMVar mfd = do
  wait <- withMVar mfd $ \fd-> do
    when (fd < 0) $ throwIO (SocketException eBADF)
    threadWaitWriteSTM fd >>= return . atomically . fst
  wait `onException` throwIO (SocketException eBADF)

threadWaitWrite' :: Fd -> IO (IO ())
threadWaitWrite' fd = do
  threadWaitWriteSTM fd >>= return . atomically . fst

threadWaitRead' :: Fd -> IO (IO ())
threadWaitRead' fd = do
  threadWaitReadSTM fd >>= return . atomically . fst
