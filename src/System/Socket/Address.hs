{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.Socket.Address
  ( Address (..)
  ) where

import Foreign.C.Types
import Foreign.Storable

class (Storable f) => Address f where
  addressFamilyNumber :: f -> CInt
