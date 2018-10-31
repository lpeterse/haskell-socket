{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Internal.Message
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Internal.Message (
    MessageFlags (..)
  , msgEndOfRecord
  , msgNoSignal
  , msgOutOfBand
  , msgWaitAll
  , msgPeek
  ) where

import Data.Bits
import Data.Monoid
import Data.Maybe
import Data.List (intersperse)
import Data.Semigroup as Sem

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

instance Sem.Semigroup MessageFlags where
  (<>) = (.|.)

instance Monoid MessageFlags where
  mempty  = MessageFlags 0
  mappend = (Sem.<>)

instance Show MessageFlags where
  show msg = "mconcat [" ++ y ++ "]"
    where
      x = [ if msg .&. msgEndOfRecord /= mempty then Just "msgEndOfRecord" else Nothing
          , if msg .&. msgNoSignal    /= mempty then Just "msgNoSignal"    else Nothing
          , if msg .&. msgOutOfBand   /= mempty then Just "msgOutOfBand"   else Nothing
          , if msg .&. msgWaitAll     /= mempty then Just "msgWaitAll"     else Nothing
          , if msg .&. msgWaitAll     /= mempty then Just "msgPeek"        else Nothing
          , let (MessageFlags i) = msg `xor` (Data.Monoid.mconcat [msgEndOfRecord,msgNoSignal,msgOutOfBand,msgWaitAll,msgPeek] .&. msg)
            in if                   i /= 0      then Just ("MessageFlags " ++ show i) else Nothing
          ]
      y = concat $ intersperse "," $ catMaybes x

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
--
--   __/It is safe and advised to always use this flag unless one wants to/__
--   __/explictly hook and handle the @PIPE@ signal which is not very useful in todays/__
--   __/multi-threaded environments anyway. Although GHC's RTS ignores the/__
--   __/signal by default it causes an unnecessary interruption./__
msgNoSignal         :: MessageFlags
msgNoSignal          = MessageFlags (#const MSG_NOSIGNAL)

-- | @MSG_EOR@
--
--   Used by `System.Socket.Type.SequentialPacket.SequentialPacket` to mark record boundaries.
--   Consult the POSIX standard for details.
msgEndOfRecord      :: MessageFlags
msgEndOfRecord       = MessageFlags (#const MSG_EOR)
{-# WARNING msgEndOfRecord "Untested: Use at your own risk!" #-}

-- | @MSG_OOB@
--
--   Used to send and receive out-of-band data. Consult the relevant standards
--   for details.
msgOutOfBand        :: MessageFlags
msgOutOfBand         = MessageFlags (#const MSG_OOB)
{-# WARNING msgOutOfBand "Untested: Use at your own risk!" #-}

-- | @MSG_WAITALL@
--
--   A `System.Socket.receive` call shall not return unless the requested number of
--   bytes becomes available.
msgWaitAll          :: MessageFlags
msgWaitAll           = MessageFlags (#const MSG_WAITALL)
{-# WARNING msgWaitAll "Untested: Use at your own risk!" #-}

-- | @MSG_PEEK@
--
--   A `System.Socket.receive` shall not actually remove the received
--   data from the input buffer.
msgPeek             :: MessageFlags
msgPeek              = MessageFlags (#const MSG_PEEK)
