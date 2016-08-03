{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Internal.Socket (
    Socket (..)
  , SocketAddress
  , Family (..)
  , Type (..)
  , Protocol (..)
  , SocketOption (..)
  , unsafeGetSocketOption
  , unsafeSetSocketOption
  , Error (..)
  , ReuseAddress (..)
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Applicative ((<$>))

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import System.Posix.Types

import System.Socket.Internal.Platform
import System.Socket.Internal.Exception

#include "hs_socket.h"

-- | A generic socket type. Use `System.Socket.socket` to create a new socket.
--
--   The socket is just an `Control.Concurrent.MVar.MVar`-wrapped file descriptor.
--   The `System.Socket.Unsafe.Socket` constructor is exported trough the unsafe
--   module in order to make  this library easily extensible, but it is usually
--   not necessary nor advised to work directly on the file descriptor.
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
--     does not separate between registering the file descriptor (for which
--     the lock __must__ be held) and the actual waiting (for which you must
--     __not__ hold the lock).
--     Also see [this](https://mail.haskell.org/pipermail/haskell-cafe/2014-September/115823.html)
--     thread and read the library code to see how the problem is currently circumvented.
newtype Socket f t p
      = Socket (MVar Fd)

-- | The `SocketAddress` type is a [data family](https://wiki.haskell.org/GHC/Type_families#Detailed_definition_of_data_families).
--   This allows to provide different data constructors depending on the socket
--   family wihtout knowing all of them in advance or the need to patch this
--   core library.
--
-- > SocketAddressInet  inetLoopback  8080     :: SocketAddress Inet
-- > SocketAddressInet6 inet6Loopback 8080 0 0 :: SocketAddress Inet6
data family SocketAddress f

class Family f where
  familyNumber :: f -> CInt

class Type t where
  typeNumber :: t -> CInt

class Protocol  p where
  protocolNumber :: p -> CInt

class SocketOption o where
  getSocketOption :: Socket f t p -> IO o
  setSocketOption :: Socket f t p -> o -> IO ()

-- | @SO_ERROR@
data Error
   = Error SocketException
   deriving (Eq, Ord, Show)

instance SocketOption Error where
  getSocketOption s =
    Error . SocketException Control.Applicative.<$> unsafeGetSocketOption s (#const SOL_SOCKET) (#const SO_ERROR)
  setSocketOption _ _ = throwIO eInvalid

-- | @SO_REUSEADDR@
data ReuseAddress
   = ReuseAddress Bool
   deriving (Eq, Ord, Show)

instance SocketOption ReuseAddress where
  getSocketOption s =
    ReuseAddress . ((/=0) :: CInt -> Bool) <$> unsafeGetSocketOption s (#const SOL_SOCKET) (#const SO_REUSEADDR)
  setSocketOption s (ReuseAddress o) =
    unsafeSetSocketOption s (#const SOL_SOCKET) (#const SO_REUSEADDR) (if o then 1 else 0 :: CInt)

-------------------------------------------------------------------------------
-- Unsafe helpers
-------------------------------------------------------------------------------

unsafeSetSocketOption :: Storable a => Socket f t p -> CInt -> CInt -> a -> IO ()
unsafeSetSocketOption (Socket mfd) level name value =
  withMVar mfd $ \fd-> alloca $ \vPtr-> alloca $ \errPtr-> do
    poke vPtr value
    i <- c_setsockopt fd level name vPtr (fromIntegral $ sizeOf value) errPtr
    when (i < 0) (SocketException <$> peek errPtr >>= throwIO)

unsafeGetSocketOption :: Storable a => Socket f t p -> CInt -> CInt -> IO a
unsafeGetSocketOption (Socket mfd) level name =
  withMVar mfd $ \fd-> alloca $ \vPtr-> alloca $ \lPtr-> alloca $ \errPtr-> do
    u <- return undefined
    poke lPtr (fromIntegral $ sizeOf u)
    i <- c_getsockopt fd level name vPtr (lPtr :: Ptr CInt) errPtr
    when (i < 0) (SocketException <$> peek errPtr >>= throwIO)
    x <- peek vPtr
    return (x `asTypeOf` u)
