{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.Socket.Family
  ( Family (..)
  , Address
  ) where

import Foreign.C.Types

class Family f where
  familyNumber :: f -> CInt

data family Address f
