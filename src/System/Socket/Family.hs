{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.Socket.Family
  ( Family (..)
  ) where

import Foreign.C.Types
import Foreign.Storable

class (Storable (Address f)) => Family f where
  type Address f
  familyNumber :: f -> CInt
