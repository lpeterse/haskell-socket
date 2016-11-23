{-# LANGUAGE DeriveDataTypeable         #-}
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

import Data.Typeable
import System.Socket.Internal.Socket

#include "hs_socket.h"

data TCP
  deriving (Typeable)

instance Protocol  TCP where
  protocolNumber _ = (#const IPPROTO_TCP)
