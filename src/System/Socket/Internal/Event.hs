{-# LANGUAGE CPP #-}
module System.Socket.Internal.Event
  ( threadWaitWrite', threadWaitRead'
  ) where

import GHC.Conc (threadWaitReadSTM, threadWaitWriteSTM, threadDelay, atomically)

import System.Posix.Types ( Fd(..) )

-------------------------------------------------------------------------------
-- Helpers for threadsafe event registration on file descriptors
-------------------------------------------------------------------------------

threadWaitWrite' :: Fd -> IO (IO ())
threadWaitWrite' fd = do
#if defined(_WIN32)
  return (threadDelay 500000)
#else
  threadWaitWriteSTM fd >>= return . atomically . fst
#endif

threadWaitRead' :: Fd -> IO (IO ())
threadWaitRead' fd = do
#if defined(_WIN32)
  return (threadDelay 500000)
#else
  threadWaitReadSTM fd >>= return . atomically . fst
#endif
