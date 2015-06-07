{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.Socket.Family
  ( Family (..)
  ) where

import Foreign.C.Types
import Foreign.Storable

class (Storable (SockAddr f)) => Family f where
  type SockAddr f
  familyNumber :: f -> CInt
