module System.Socket.Options where

import Control.Exception
import Control.Concurrent.MVar

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.C.Error

import System.Socket.Exception
import System.Socket.Internal

#include "sys/socket.h"

class GetSockOpt o where
  getSockOpt :: Socket f t p -> IO o

class SetSockOpt o where
  setSockOpt :: Socket f t p -> o -> IO ()

data SO_ACCEPTCONN
   = SO_ACCEPTCONN Bool

instance GetSockOpt SO_ACCEPTCONN where
  getSockOpt (Socket mfd) = do
    withMVar mfd $ \fd->
      alloca $ \vPtr-> do
        alloca $ \lPtr-> do
          i <- c_getsockopt fd (#const SOL_SOCKET) (#const SO_ACCEPTCONN) (vPtr :: Ptr Int) (lPtr :: Ptr Int)
          if i < 0 then do
            throwIO . SocketException =<< getErrno
          else do
            v <- peek vPtr
            return $ SO_ACCEPTCONN (v == 1)