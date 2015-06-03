{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.Socket.Family
  ( Family (..)
  , SockAddr
  ) where

import Foreign.C.Types
import Foreign.Storable

class (SockAddr (Address f)) => Family f where
  type Address f
  familyNumber :: f -> CInt

class Storable a => SockAddr a