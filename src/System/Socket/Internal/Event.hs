module System.Socket.Internal.Event
  ( threadWaitWrite', threadWaitRead'
  ) where

import GHC.Conc (threadWaitReadSTM, threadWaitWriteSTM, atomically)

import System.Posix.Types ( Fd(..) )

-------------------------------------------------------------------------------
-- Helpers for threadsafe event registration on file descriptors
-------------------------------------------------------------------------------

threadWaitWrite' :: Fd -> IO (IO ())
threadWaitWrite' fd = do
  threadWaitWriteSTM fd >>= return . atomically . fst

threadWaitRead' :: Fd -> IO (IO ())
threadWaitRead' fd = do
  threadWaitReadSTM fd >>= return . atomically . fst
