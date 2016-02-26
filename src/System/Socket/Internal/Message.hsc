{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Socket.Internal.Message (
    MessageFlags (..)
  , Message
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

instance Monoid MessageFlags where
  mempty  = MessageFlags 0
  mappend = (.|.)

instance Show MessageFlags where
  show msg = "mconcat [" ++ y ++ "]"
    where
      x = [ if msg .&. msgEndOfRecord /= mempty then Just "msgEndOfRecord" else Nothing
          , if msg .&. msgNoSignal    /= mempty then Just "msgNoSignal"    else Nothing
          , if msg .&. msgOutOfBand   /= mempty then Just "msgOutOfBand"   else Nothing
          , if msg .&. msgWaitAll     /= mempty then Just "msgWaitAll"     else Nothing
          , let (MessageFlags i) = msg `xor` (Data.Monoid.mconcat [msgEndOfRecord,msgNoSignal,msgOutOfBand,msgWaitAll] .&. msg)
            in if                   i /= 0      then Just ("MessageFlags " ++ show i) else Nothing
          ]
      y = concat $ intersperse "," $ catMaybes x

-- | @MSG_EOR@
msgEndOfRecord      :: MessageFlags
msgEndOfRecord       = MessageFlags (#const MSG_EOR)

-- | @MSG_NOSIGNAL@
--
--   Suppresses the generation of @PIPE@ signals when writing to a socket
--   that is no longer connected.
--
--   Although this flag is POSIX, it is not available on all platforms. Try
--
--   > msgNoSignal /= mempty
--
--   in order to check whether this flag is defined on a certain platform.
--   It is safe to just use this constant even if it might not have effect
--   on a certain target platform. The platform independence of this flag
--   is therefore fulfilled to some extent.
--
--   Some more explanation on the platform specific behaviour:
--
--   - Linux defines and supports `MSG_NOSIGNAL` and properly suppresses
--     the generation of broken pipe-related signals.
--   - Windows does not define it, but does not generate signals either.
--   - OSX does not define it, but generates @PIPE@ signals. The GHC runtime
--     ignores them if you don't hook them explicitly. The
--     non-portable socket option `SO_NOSIGPIPE` may be used disable signals
--     on a per-socket basis.
msgNoSignal         :: MessageFlags
msgNoSignal          = MessageFlags (#const MSG_NOSIGNAL)

-- | @MSG_OOB@
msgOutOfBand        :: MessageFlags
msgOutOfBand         = MessageFlags (#const MSG_OOB)

-- | @MSG_WAITALL@
msgWaitAll          :: MessageFlags
msgWaitAll           = MessageFlags (#const MSG_WAITALL)
