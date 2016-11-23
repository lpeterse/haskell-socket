{-# LANGUAGE DeriveDataTypeable         #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Type.Datagram
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Type.Datagram where

import Data.Typeable
import System.Socket.Internal.Socket

#include "hs_socket.h"

data Datagram
  deriving (Typeable)

instance Type Datagram where
  typeNumber _ = (#const SOCK_DGRAM)
