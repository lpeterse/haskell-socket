module System.Socket.Internal.Socket (
    Socket (..)
  , GetSockOpt (..)
  , SetSockOpt (..)
  , SO_ACCEPTCONN (..)
  ) where

import Control.Concurrent.MVar
import Control.Exception

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Error
import Foreign.Marshal.Alloc
import System.Posix.Types

import System.Socket.Internal.FFI
import System.Socket.Internal.Exception

#include "sys/socket.h"

-- | A generic socket type. Also see `socket` for details.
--
--   The socket is just an `Control.Concurrent.MVar.MVar`-wrapped file descriptor.
--   It is exposed in order to make this library easily extensible, but it is
--   usually not necessary nor advised to work directly on the file descriptor.
--   If you do, the following rules must be obeyed:
--
--   - Make sure not to deadlock. Use `Control.Concurrent.MVar.withMVar` or similar.
--   - The lock __must not__ be held during a blocking call. This would make it impossible
--     to send and receive simultaneously or to close the socket.
--   - The lock __must__ be held when calling operations that use the file descriptor.
--     Otherwise the socket might get closed or even reused by another
--     thread/capability which might result in reading from or writing
--     totally different connection. This is a security nightmare!
--   - The socket is non-blocking and all the code relies on that assumption.
--     You need to use GHC's eventing mechanism primitives to block until
--     something happens. The former rules forbid to use `GHC.Conc.threadWaitRead` as it
--     does not seperate between registering the file descriptor (for which
--     the lock __must__ be held) and the actual waiting (for which you must
--     __not__ hold the lock).
--     Also see [this](https://mail.haskell.org/pipermail/haskell-cafe/2014-September/115823.html)
--     thread and read the library code to see how the problem is currently circumvented.
newtype Socket d t p
      = Socket (MVar Fd)

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