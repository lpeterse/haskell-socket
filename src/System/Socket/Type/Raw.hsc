--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Type.Raw where

import System.Socket.Internal.Socket

#include "hs_socket.h"

data Raw

instance Type Raw where
  typeNumber _ = (#const SOCK_RAW)
