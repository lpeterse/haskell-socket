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
    | e == eOk                   = "eOk"
    | e == eInterrupted          = "eInterrupted"
    | e == eAgain                = "eAgain"
    | e == eWouldBlock           = "eWouldBlock"
    | e == eBadFileDescriptor    = "eBadFileDescriptor"
    | e == eInProgress           = "eInProgress"
    | e == eProtocolNotSupported = "eProtocolNotSupported"
    | e == eInvalid              = "eInvalid"
    | e == eConnectionRefused    = "eConnectionRefused"
    | e == eNetworkUnreachable   = "eNetworkUnreachable"
    | e == eNotConnected         = "eNotConnected"
    | e == eAlready              = "eAlready"
    | e == eIsConnected          = "eIsConnected"
    | e == eTimedOut             = "eTimedOut"
    | e == ePipe                 = "ePipe"
    | otherwise                  = "SocketException " ++ show i

eOk                       :: SocketException
eOk                        = SocketException (#const SEOK)

eInterrupted              :: SocketException
eInterrupted               = SocketException (#const SEINTR)

eAgain                    :: SocketException
eAgain                     = SocketException (#const SEAGAIN)

eWouldBlock               :: SocketException
eWouldBlock                = SocketException (#const SEWOULDBLOCK)

eBadFileDescriptor        :: SocketException
eBadFileDescriptor         = SocketException (#const SEBADF)

eInProgress               :: SocketException
eInProgress                = SocketException (#const SEINPROGRESS)

eProtocolNotSupported     :: SocketException
eProtocolNotSupported      = SocketException (#const SEPROTONOSUPPORT)

eInvalid                  :: SocketException
eInvalid                   = SocketException (#const SEINVAL)

eConnectionRefused        :: SocketException
eConnectionRefused         = SocketException (#const SECONNREFUSED)

eNetworkUnreachable       :: SocketException
eNetworkUnreachable        = SocketException (#const SENETUNREACH)

eNotConnected             :: SocketException
eNotConnected              = SocketException (#const SENOTCONN)

eAlready                  :: SocketException
eAlready                   = SocketException (#const SEALREADY)

eIsConnected              :: SocketException
eIsConnected               = SocketException (#const SEISCONN)

eTimedOut                 :: SocketException
eTimedOut                  = SocketException (#const SETIMEDOUT)

ePipe                     :: SocketException
ePipe                      = SocketException (#const SEPIPE)
