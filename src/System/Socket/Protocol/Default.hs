--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Protocol.Default
-- Copyright   :  (c) Lars Petersen 2016
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Protocol.Default where

import Data.Typeable
import System.Socket.Internal.Socket

data Default
  deriving (Typeable)

instance Protocol Default where
  protocolNumber _ = 0
