{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.Socket.Address
  ( Address (..)
  ) where

import Foreign.C.Types
import Foreign.Storable

class (Storable a) => Address a where
  addressFamilyNumber :: a -> CInt
