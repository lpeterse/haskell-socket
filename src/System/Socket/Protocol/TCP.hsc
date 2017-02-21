--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Protocol.TCP
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Protocol.TCP where

import Foreign.C.Types

import System.Socket.Internal.Socket
import System.Socket.Internal.SocketOption

#include "hs_socket.h"

data TCP

instance Protocol TCP where
  protocolNumber _ = (#const IPPROTO_TCP)

-- | If set to True, disable the Nagle's algorithm.
--
--  - Also know as @TCP_NODELAY@.
data NoDelay
  = NoDelay Bool
  deriving (Eq, Ord, Show)

instance SocketOption NoDelay where
  getSocketOption s =
    NoDelay . ((/=0) :: CInt -> Bool) <$> unsafeGetSocketOption s (#const IPPROTO_TCP) (#const TCP_NODELAY)
  setSocketOption s (NoDelay o) =
    unsafeSetSocketOption s (#const IPPROTO_TCP) (#const TCP_NODELAY) (if o then 1 else 0 :: CInt)
