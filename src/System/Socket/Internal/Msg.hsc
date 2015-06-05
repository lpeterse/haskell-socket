{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Socket.Internal.Msg (
    MsgFlags (..)
  , Msg
  , IoVec
  , msgDONTWAIT
  , msgEOR
  , msgMORE
  , msgNOSIGNAL
  , msgOOB
  , msgTRUNC
  , msgWAITALL
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
--   > mconcat [msgNOSIGNAL, msgWAITALL]
--
--   Use the `Data.Bits.Bits` instance to check whether a flag is set:
--
--   > if flags .&. msgEOR /= mempty then ...
newtype MsgFlags
      = MsgFlags CInt
      deriving (Eq, Bits, Storable)

data Msg a t p

data IoVec

instance Monoid MsgFlags where
  mempty  = MsgFlags 0
  mappend = (.|.)

instance Show MsgFlags where
  show msg = "mconcat [" ++ y ++ "]"
    where
      x = [ if msg .&. msgEOR      /= mempty then Just "msgEOR"      else Nothing
          , if msg .&. msgNOSIGNAL /= mempty then Just "msgNOSIGNAL" else Nothing
          , if msg .&. msgOOB      /= mempty then Just "msgOOB"      else Nothing
          , if msg .&. msgWAITALL  /= mempty then Just "msgWAITALL"  else Nothing
          , if msg .&. msgTRUNC    /= mempty then Just "msgTRUNC"    else Nothing
          , if msg .&. msgMORE     /= mempty then Just "msgMORE"     else Nothing
          , if msg .&. msgDONTWAIT /= mempty then Just "msgDONTWAIT" else Nothing
          , let (MsgFlags i) = msg `xor` (mconcat [msgEOR,msgNOSIGNAL,msgOOB,msgWAITALL,msgTRUNC,msgMORE,msgDONTWAIT] .&. msg)
            in if i /= 0 then Just ("MsgFlags " ++ show i) else Nothing 
          ]
      y = concat $ intersperse "," $ catMaybes x

msgEOR      :: MsgFlags
msgEOR       = MsgFlags (#const MSG_EOR)

msgNOSIGNAL :: MsgFlags
msgNOSIGNAL  = MsgFlags (#const MSG_NOSIGNAL)

msgOOB      :: MsgFlags
msgOOB       = MsgFlags (#const MSG_OOB)

msgWAITALL  :: MsgFlags
msgWAITALL   = MsgFlags (#const MSG_WAITALL)

msgTRUNC    :: MsgFlags
msgTRUNC     = MsgFlags (#const MSG_TRUNC)

msgMORE     :: MsgFlags
msgMORE      = MsgFlags (#const MSG_MORE)

msgDONTWAIT :: MsgFlags
msgDONTWAIT  = MsgFlags (#const MSG_DONTWAIT)

