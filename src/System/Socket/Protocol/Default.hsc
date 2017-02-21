--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Protocol.Default
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Protocol.Default where

import System.Socket.Internal.Socket

#include "hs_socket.h"

data Default

instance Protocol Default where
  protocolNumber _ = 0
