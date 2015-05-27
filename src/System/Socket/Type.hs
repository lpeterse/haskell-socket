{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.Socket.Type
  ( Type (..)
  ) where

import Foreign.C.Types

class Type t where
  typeNumber :: t -> CInt