--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Protocol.TCP where

import System.Socket.Internal.Socket

#include "hs_socket.h"

data TCP

instance Protocol  TCP where
  protocolNumber _ = (#const IPPROTO_TCP)
