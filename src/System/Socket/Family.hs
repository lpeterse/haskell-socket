{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.Socket.Family
  ( Family (..)
  ) where

import Foreign.C.Types
import Foreign.Storable

class (Storable (SocketAddress f)) => Family f where
  type SocketAddress f
  familyNumber :: f -> CInt
