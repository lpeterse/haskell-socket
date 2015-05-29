module System.Socket.Internal.MsgFlags (
    MsgFlags (..)
  , msgEOR
  , msgNOSIGNAL
  , msgOOB
  , msgWAITALL
  ) where

import Data.Bits
import Data.Monoid

import Foreign.C.Types

#include "sys/socket.h"

-- | Use the `Data.Monoid.Monoid` instance to combine several flags:
--
--   > mconcat [msgNOSIGNAL, msgWAITALL]
newtype MsgFlags
      = MsgFlags CInt
      deriving (Eq, Show)

instance Monoid MsgFlags where
  mempty
    = MsgFlags 0
  mappend (MsgFlags a) (MsgFlags b)
    = MsgFlags (a .|. b)

msgEOR      :: MsgFlags
msgEOR       = MsgFlags (#const MSG_EOR)

msgNOSIGNAL :: MsgFlags
msgNOSIGNAL  = MsgFlags (#const MSG_NOSIGNAL)

msgOOB      :: MsgFlags
msgOOB       = MsgFlags (#const MSG_OOB)

msgWAITALL  :: MsgFlags
msgWAITALL   = MsgFlags (#const MSG_WAITALL)

