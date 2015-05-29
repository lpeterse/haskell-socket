module System.Socket.Internal.MsgFlags (
    MsgFlags (..)
  , msgEOR
  , msgOOB
  , msgNOSIGNAL
  ) where

import Data.Bits
import Data.Monoid

import Foreign.C.Types

#include "sys/socket.h"

newtype MsgFlags
      = MsgFlags CInt

instance Monoid MsgFlags where
  mempty
    = MsgFlags 0
  mappend (MsgFlags a) (MsgFlags b)
    = MsgFlags (a .|. b)

msgOOB      :: MsgFlags
msgOOB       = MsgFlags (#const MSG_NOSIGNAL)

msgEOR      :: MsgFlags
msgEOR       = MsgFlags (#const MSG_NOSIGNAL)

msgNOSIGNAL :: MsgFlags
msgNOSIGNAL  = MsgFlags (#const MSG_NOSIGNAL)

