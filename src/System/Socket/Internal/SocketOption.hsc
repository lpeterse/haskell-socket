--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Internal.SocketOption
-- Copyright   :  (c) Lars Petersen 2016
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Internal.SocketOption (
    SocketOption (..)
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

import System.Socket.Internal.Socket
import System.Socket.Internal.Platform
import System.Socket.Internal.Exception

#include "hs_socket.h"

-- | `SocketOption`s allow to read and write certain properties of a socket.
--
--   - Each option shall have a corresponding data type that models the data
--     associated with the socket option.
--   - Use `System.Socket.Unsafe.unsafeGetSocketOption` and
--    `System.Socket.Unsafe.unsafeSetSocketOption` in order to implement custom socket
--     options.
class SocketOption o where
  -- | Get a specific `SocketOption`.
  --
  --   - This operation throws `SocketException`s. Consult @man getsockopt@ for
  --     details and specific errors.
  getSocketOption :: Socket f t p -> IO o
  -- | Set a specific `SocketOption`.
  --
  --   - This operation throws `SocketException`s. Consult @man setsockopt@ for
  --     details and specific errors.
  setSocketOption :: Socket f t p -> o -> IO ()

-- | Reports the last error that occured on the socket.
--
--   - Also known as @SO_ERROR@.
--   - The operation `setSocketOption` always throws `eInvalid` for  this option.
--   - Use with care in the presence of concurrency!
data Error
   = Error SocketException
   deriving (Eq, Ord, Show)


instance SocketOption Error where
  getSocketOption s =
    Error . SocketException Control.Applicative.<$> unsafeGetSocketOption s (#const SOL_SOCKET) (#const SO_ERROR)
  setSocketOption _ _ = throwIO eInvalid

-- | Allows or disallows the reuse of a local address in a `System.Socket.bind` call.
--
--  - Also known as @SO_REUSEADDR@.
--  - This is particularly useful when experiencing `eAddressInUse` exceptions.
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
