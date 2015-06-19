{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Socket.Internal.Message (
    MessageFlags (..)
  , Message
  , IoVec
  , msgEndOfRecord
  , msgNoSignal
  , msgOutOfBand
  , msgWaitAll
  ) where

import Data.Bits
import Data.Monoid
import Data.Maybe
import Data.List (intersperse)

import Foreign.C.Types
import Foreign.Storable

#include "hs_socket.h"

-- | Use the `Data.Monoid.Monoid` instance to combine several flags:
--
--   > mconcat [msgNoSignal, msgWaitAll]
--
--   Use the `Data.Bits.Bits` instance to check whether a flag is set:
--
--   > if flags .&. msgEndOfRecord /= mempty then ...
newtype MessageFlags
      = MessageFlags CInt
      deriving (Eq, Bits, Storable)

data Message a t p

data IoVec

instance Monoid MessageFlags where
  mempty  = MessageFlags 0
  mappend = (.|.)

instance Show MessageFlags where
  show msg = "mconcat [" ++ y ++ "]"
    where
      x = [ if msg .&. msgEndOfRecord      /= mempty then Just "msgEndOfRecord"      else Nothing
          , if msg .&. msgNoSignal /= mempty then Just "msgNoSignal" else Nothing
          , if msg .&. msgOutOfBand      /= mempty then Just "msgOutOfBand"      else Nothing
          , if msg .&. msgWaitAll  /= mempty then Just "msgWaitAll"  else Nothing
          , let (MessageFlags i) = msg `xor` (mconcat [msgEndOfRecord,msgNoSignal,msgOutOfBand,msgWaitAll] .&. msg)
            in if i /= 0 then Just ("MessageFlags " ++ show i) else Nothing 
          ]
      y = concat $ intersperse "," $ catMaybes x

-- | @MSG_EOR@
msgEndOfRecord      :: MessageFlags
msgEndOfRecord       = MessageFlags (#const MSG_EOR)

-- | @MSG_NOSIGNAL@
msgNoSignal         :: MessageFlags
msgNoSignal          = MessageFlags (#const MSG_NOSIGNAL)

-- | @MSG_OOB@
msgOutOfBand        :: MessageFlags
msgOutOfBand         = MessageFlags (#const MSG_OOB)

-- | @MSG_WAITALL@
msgWaitAll          :: MessageFlags
msgWaitAll           = MessageFlags (#const MSG_WAITALL)
