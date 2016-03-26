{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module System.Socket.Internal.Exception where

import Control.Exception
import Data.Typeable
import Foreign.C.Types

#include "hs_socket.h"

newtype SocketException
      = SocketException CInt
  deriving (Typeable, Eq, Ord)

instance Exception SocketException

instance Show SocketException where
  show e@(SocketException i)
    | e == eOk                         = "eOk"
    | e == eInterrupted                = "eInterrupted"
    | e == eBadFileDescriptor          = "eBadFileDescriptor"
    | e == eInvalid                    = "eInvalid"
    | e == ePipe                       = "ePipe"
    | e == eWouldBlock                 = "eWouldBlock"
    | e == eAgain                      = "eAgain"
    | e == eNotSocket                  = "eNotSocket"
    | e == eDestinationAddressRequired = "eDestinationAddressRequired"
    | e == eMessageSize                = "eMessageSize"
    | e == eProtocolType               = "eProtocolType"
    | e == eNoProtocolOption           = "eNoProtocolOption"
    | e == eProtocolNotSupported       = "eProtocolNotSupported"
    | e == eSocketTypeNotSupported     = "eSocketTypeNotSupported"
    | e == eOperationNotSupported      = "eOperationNotSupported"
    | e == eProtocolFamilyNotSupported = "eProtocolFamilyNotSupported"
    | e == eAddressFamilyNotSupported  = "eAddressFamilyNotSupported"
    | e == eAddressInUse               = "eAddressInUse"
    | e == eAddressNotAvailable        = "eAddressNotAvailable"
    | e == eNetworkDown                = "eNetworkDown"
    | e == eNetworkUnreachable         = "eNetworkUnreachable"
    | e == eNetworkReset               = "eNetworkReset"
    | e == eConnectionAborted          = "eConnectionAborted"
    | e == eConnectionReset            = "eConnectionReset"
    | e == eNoBufferSpace              = "eNoBufferSpace"
    | e == eIsConnected                = "eIsConnected"
    | e == eNotConnected               = "eNotConnected"
    | e == eShutdown                   = "eShutdown"
    | e == eTooManyReferences          = "eTooManyReferences"
    | e == eTimedOut                   = "eTimedOut"
    | e == eConnectionRefused          = "eConnectionRefused"
    | e == eHostDown                   = "eHostDown"
    | e == eHostUnreachable            = "eHostUnreachable"
    | e == eAlready                    = "eAlready"
    | e == eInProgress                 = "eInProgress"
    | otherwise                        = "SocketException " ++ show i

eOk                         :: SocketException
eOk                          = SocketException (#const SEOK)

eInterrupted                :: SocketException
eInterrupted                 = SocketException (#const SEINTR)

eBadFileDescriptor          :: SocketException
eBadFileDescriptor           = SocketException (#const SEBADF)

eInvalid                    :: SocketException
eInvalid                     = SocketException (#const SEINVAL)

ePipe                       :: SocketException
ePipe                        = SocketException (#const SEPIPE)

eWouldBlock                 :: SocketException
eWouldBlock                  = SocketException (#const SEWOULDBLOCK)

eAgain                      :: SocketException
eAgain                       = SocketException (#const SEAGAIN)

eNotSocket                  :: SocketException
eNotSocket                   = SocketException (#const SENOTSOCK)

eDestinationAddressRequired :: SocketException
eDestinationAddressRequired  = SocketException (#const SEDESTADDRREQ)

eMessageSize                :: SocketException
eMessageSize                 = SocketException (#const SEMSGSIZE)

eProtocolType               :: SocketException
eProtocolType                = SocketException (#const SEPROTOTYPE)

eNoProtocolOption           :: SocketException
eNoProtocolOption            = SocketException (#const SENOPROTOOPT)

eProtocolNotSupported       :: SocketException
eProtocolNotSupported        = SocketException (#const SEPROTONOSUPPORT)

eSocketTypeNotSupported     :: SocketException
eSocketTypeNotSupported      = SocketException (#const SESOCKTNOSUPPORT)

eOperationNotSupported      :: SocketException
eOperationNotSupported       = SocketException (#const SEOPNOTSUPP)

eProtocolFamilyNotSupported :: SocketException
eProtocolFamilyNotSupported  = SocketException (#const SEPFNOSUPPORT)

eAddressFamilyNotSupported  :: SocketException
eAddressFamilyNotSupported   = SocketException (#const SEAFNOSUPPORT)

eAddressInUse               :: SocketException
eAddressInUse                = SocketException (#const SEADDRINUSE)

eAddressNotAvailable        :: SocketException
eAddressNotAvailable         = SocketException (#const SEADDRNOTAVAIL)

eNetworkDown                :: SocketException
eNetworkDown                 = SocketException (#const SENETDOWN)

eNetworkUnreachable         :: SocketException
eNetworkUnreachable          = SocketException (#const SENETUNREACH)

eNetworkReset               :: SocketException
eNetworkReset                = SocketException (#const SENETRESET)

eConnectionAborted          :: SocketException
eConnectionAborted           = SocketException (#const SECONNABORTED)

eConnectionReset            :: SocketException
eConnectionReset             = SocketException (#const SECONNRESET)

eNoBufferSpace              :: SocketException
eNoBufferSpace               = SocketException (#const SENOBUFS)

eIsConnected                :: SocketException
eIsConnected                 = SocketException (#const SEISCONN)

eNotConnected               :: SocketException
eNotConnected                = SocketException (#const SENOTCONN)

eShutdown                   :: SocketException
eShutdown                    = SocketException (#const SESHUTDOWN)

eTooManyReferences          :: SocketException
eTooManyReferences           = SocketException (#const SETOOMANYREFS)

eTimedOut                   :: SocketException
eTimedOut                    = SocketException (#const SETIMEDOUT)

eConnectionRefused          :: SocketException
eConnectionRefused           = SocketException (#const SECONNREFUSED)

eHostDown                   :: SocketException
eHostDown                    = SocketException (#const SEHOSTDOWN)

eHostUnreachable            :: SocketException
eHostUnreachable             = SocketException (#const SEHOSTUNREACH)

eAlready                    :: SocketException
eAlready                     = SocketException (#const SEALREADY)

eInProgress                 :: SocketException
eInProgress                  = SocketException (#const SEINPROGRESS)
