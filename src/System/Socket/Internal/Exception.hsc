{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Internal.Exception
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Internal.Exception
  ( SocketException (..)
  , eOk
  , eInterrupted
  , eBadFileDescriptor
  , eInvalid
  , ePipe
  , eWouldBlock
  , eAgain
  , eNotSocket
  , eDestinationAddressRequired
  , eMessageSize
  , eProtocolType
  , eNoProtocolOption
  , eProtocolNotSupported
  , eSocketTypeNotSupported
  , eOperationNotSupported
  , eProtocolFamilyNotSupported
  , eAddressFamilyNotSupported
  , eAddressInUse
  , eAddressNotAvailable
  , eNetworkDown
  , eNetworkUnreachable
  , eNetworkReset
  , eConnectionAborted
  , eConnectionReset
  , eNoBufferSpace
  , eIsConnected
  , eNotConnected
  , eShutdown
  , eTooManyReferences
  , eTimedOut
  , eConnectionRefused
  , eHostDown
  , eHostUnreachable
  , eAlready
  , eInProgress
  ) where

import Control.Exception
import Data.Typeable
import Foreign.C.Types

#include "hs_socket.h"

-- | Contains the error code that can be matched against.
--
--   Hint: Use guards or @MultiWayIf@ to match against specific exceptions:
--
--   > if | e == eAddressInUse -> ...
--   >    | e == eAddressNotAvailable -> ...
--   >    | otherwise -> ...
newtype SocketException
      = SocketException CInt
  deriving (Typeable, Eq, Ord)

instance Exception SocketException

instance Show SocketException where
  show e
    | e == eOk                           = "eOk"
    | e == eInterrupted                  = "eInterrupted"
    | e == eBadFileDescriptor            = "eBadFileDescriptor"
    | e == eInvalid                      = "eInvalid"
    | e == ePipe                         = "ePipe"
    | e == eWouldBlock                   = "eWouldBlock"
    | e == eAgain                        = "eAgain"
    | e == eNotSocket                    = "eNotSocket"
    | e == eDestinationAddressRequired   = "eDestinationAddressRequired"
    | e == eMessageSize                  = "eMessageSize"
    | e == eProtocolType                 = "eProtocolType"
    | e == eNoProtocolOption             = "eNoProtocolOption"
    | e == eProtocolNotSupported         = "eProtocolNotSupported"
    | e == eSocketTypeNotSupported       = "eSocketTypeNotSupported"
    | e == eOperationNotSupported        = "eOperationNotSupported"
    | e == eProtocolFamilyNotSupported   = "eProtocolFamilyNotSupported"
    | e == eAddressFamilyNotSupported    = "eAddressFamilyNotSupported"
    | e == eAddressInUse                 = "eAddressInUse"
    | e == eAddressNotAvailable          = "eAddressNotAvailable"
    | e == eNetworkDown                  = "eNetworkDown"
    | e == eNetworkUnreachable           = "eNetworkUnreachable"
    | e == eNetworkReset                 = "eNetworkReset"
    | e == eConnectionAborted            = "eConnectionAborted"
    | e == eConnectionReset              = "eConnectionReset"
    | e == eNoBufferSpace                = "eNoBufferSpace"
    | e == eIsConnected                  = "eIsConnected"
    | e == eNotConnected                 = "eNotConnected"
    | e == eShutdown                     = "eShutdown"
    | e == eTooManyReferences            = "eTooManyReferences"
    | e == eTimedOut                     = "eTimedOut"
    | e == eConnectionRefused            = "eConnectionRefused"
    | e == eHostDown                     = "eHostDown"
    | e == eHostUnreachable              = "eHostUnreachable"
    | e == eAlready                      = "eAlready"
    | e == eInProgress                   = "eInProgress"
    | otherwise                          = let SocketException n = e
                                           in "SocketException " ++ show n

-- | No error.
eOk                         :: SocketException
eOk                          = SocketException (#const SEOK)

-- | Interrupted system call.
--
--   NOTE: This exception shall not be thrown by any public operation in this
--   library, but is handled internally.
eInterrupted                :: SocketException
eInterrupted                 = SocketException (#const SEINTR)

-- | Bad file descriptor.
eBadFileDescriptor          :: SocketException
eBadFileDescriptor           = SocketException (#const SEBADF)

-- | Invalid argument.
eInvalid                    :: SocketException
eInvalid                     = SocketException (#const SEINVAL)

-- | Broken pipe.
ePipe                       :: SocketException
ePipe                        = SocketException (#const SEPIPE)

-- | Resource temporarily unavailable.
--
--   NOTE: This exception shall not be thrown by any public operation in this
--   library, but is handled internally.
eWouldBlock                 :: SocketException
eWouldBlock                  = SocketException (#const SEWOULDBLOCK)

-- | Resource temporarily unavailable.
eAgain                      :: SocketException
eAgain                       = SocketException (#const SEAGAIN)

-- | Socket operation on non-socket.
--
--  NOTE: This should be ruled out by the type system.
eNotSocket                  :: SocketException
eNotSocket                   = SocketException (#const SENOTSOCK)

-- | Destination address required.
eDestinationAddressRequired :: SocketException
eDestinationAddressRequired  = SocketException (#const SEDESTADDRREQ)

-- | Message too long.
eMessageSize                :: SocketException
eMessageSize                 = SocketException (#const SEMSGSIZE)

-- | Protocol wrong type for socket.

--  NOTE: This should be ruled out by the type system.
eProtocolType               :: SocketException
eProtocolType                = SocketException (#const SEPROTOTYPE)

-- | Protocol not available.
eNoProtocolOption           :: SocketException
eNoProtocolOption            = SocketException (#const SENOPROTOOPT)

-- | Protocol not supported.
eProtocolNotSupported       :: SocketException
eProtocolNotSupported        = SocketException (#const SEPROTONOSUPPORT)

-- | Socket type not supported.
eSocketTypeNotSupported     :: SocketException
eSocketTypeNotSupported      = SocketException (#const SESOCKTNOSUPPORT)

-- | Operation not supported.
eOperationNotSupported      :: SocketException
eOperationNotSupported       = SocketException (#const SEOPNOTSUPP)

-- | Protocol family not supported.
eProtocolFamilyNotSupported :: SocketException
eProtocolFamilyNotSupported  = SocketException (#const SEPFNOSUPPORT)

-- | Address family not supported by protocol.
eAddressFamilyNotSupported  :: SocketException
eAddressFamilyNotSupported   = SocketException (#const SEAFNOSUPPORT)

-- | Address already in use.
eAddressInUse               :: SocketException
eAddressInUse                = SocketException (#const SEADDRINUSE)

-- | Cannot assign requested address.
eAddressNotAvailable        :: SocketException
eAddressNotAvailable         = SocketException (#const SEADDRNOTAVAIL)

-- | Network is down.
eNetworkDown                :: SocketException
eNetworkDown                 = SocketException (#const SENETDOWN)

-- | Network is unreachable.
eNetworkUnreachable         :: SocketException
eNetworkUnreachable          = SocketException (#const SENETUNREACH)

-- | Network dropped connection on reset.
eNetworkReset               :: SocketException
eNetworkReset                = SocketException (#const SENETRESET)

-- | Software caused connection abort.
eConnectionAborted          :: SocketException
eConnectionAborted           = SocketException (#const SECONNABORTED)

-- | Connection reset by peer.
eConnectionReset            :: SocketException
eConnectionReset             = SocketException (#const SECONNRESET)

-- | No buffer space available.
eNoBufferSpace              :: SocketException
eNoBufferSpace               = SocketException (#const SENOBUFS)

-- | Transport endpoint is already connected.
eIsConnected                :: SocketException
eIsConnected                 = SocketException (#const SEISCONN)

-- | Transport endpoint is not connected.
eNotConnected               :: SocketException
eNotConnected                = SocketException (#const SENOTCONN)

-- | Cannot send after transport endpoint shutdown.
eShutdown                   :: SocketException
eShutdown                    = SocketException (#const SESHUTDOWN)

-- | Too many references: cannot splice.
eTooManyReferences          :: SocketException
eTooManyReferences           = SocketException (#const SETOOMANYREFS)

-- | Connection timed out.
eTimedOut                   :: SocketException
eTimedOut                    = SocketException (#const SETIMEDOUT)

-- | Connection refused.
eConnectionRefused          :: SocketException
eConnectionRefused           = SocketException (#const SECONNREFUSED)

-- | Host is down.
eHostDown                   :: SocketException
eHostDown                    = SocketException (#const SEHOSTDOWN)

-- | No route to host.
eHostUnreachable            :: SocketException
eHostUnreachable             = SocketException (#const SEHOSTUNREACH)

-- | Operation already in progress.
--
--   NOTE: This exception shall not be thrown by any public operation in this
--   library, but is handled internally.
eAlready                    :: SocketException
eAlready                     = SocketException (#const SEALREADY)

-- | Operation now in progress
eInProgress                 :: SocketException
eInProgress                  = SocketException (#const SEINPROGRESS)
