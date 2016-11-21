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
