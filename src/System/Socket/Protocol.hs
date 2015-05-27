{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.Socket.Protocol
  ( Protocol (..)
  ) where

import Foreign.C.Types

class Protocol  p where
  protocolNumber :: p -> CInt

instance Protocol () where
  protocolNumber _ = 0