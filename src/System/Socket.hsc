module System.Socket where

import Control.Exception
import Control.Monad

import Data.Typeable

import Foreign.C.Types
import Foreign.C.Error

#include "sys/types.h"
#include "sys/socket.h"
#include "netinet/in.h"

newtype Socket f t p
      = Socket CInt

data AF_INET        = AF_INET
data AF_INET6       = AF_INET6

data SOCK_STREAM    = SOCK_STREAM
data SOCK_DGRAM     = SOCK_DGRAM
data SOCK_SEQPACKET = SOCK_SEQPACKET

data IPPROTO_TCP    = IPPROTO_TCP
data IPPROTO_SCTP   = IPPROTO_SCTP

class Family f where
  familyNumber :: f -> CInt

instance Family AF_INET where
  familyNumber _ = (#const AF_INET)

instance Family AF_INET6 where
  familyNumber _ = (#const AF_INET6)

class Type t where
  typeNumber :: t -> CInt

instance Type SOCK_STREAM where
  typeNumber _ = (#const SOCK_STREAM)

instance Type SOCK_DGRAM where
  typeNumber _ = (#const SOCK_DGRAM)

instance Type SOCK_SEQPACKET where
  typeNumber _ = (#const SOCK_SEQPACKET)

class Protocol p where
  protocolNumber :: p -> CInt

instance Protocol IPPROTO_TCP where
  protocolNumber _ = (#const IPPROTO_TCP)

instance Protocol IPPROTO_SCTP where
  protocolNumber _ = (#const IPPROTO_SCTP)

newtype SocketException = SocketException Errno
  deriving (Typeable)

instance Show SocketException where
  show (SocketException e@(Errno i))
    | e == eACCES                 = "socket: Permission to create a socket of the specified type and/or protocol is denied."
    | e == eAFNOSUPPORT           = "socket: The implementation does not support the specified address family."
    | e == eINVAL                 = "socket: Unknown protocol, or protocol family not available."
    | e == eMFILE                 = "socket: Process file table overflow."
    | e == eNFILE                 = "socket: The system limit on the total number of open files has been reached."
    | e == eNOBUFS || e == eNOMEM = "socket: Insufficient memory is available.  The socket cannot be created until sufficient resources are freed."
    | e == ePROTONOSUPPORT        = "socket: The protocol type or the specified protocol is not supported within this domain."
    | otherwise                   = "socket: errno " ++ (show i)

instance Exception SocketException

socket :: (Family f, Type t, Protocol p) => f -> t -> p -> IO (Socket f t p)
socket f t p = do
  s <- c_socket (familyNumber f) (typeNumber t) (protocolNumber p)
  when (s == -1) $ do
    e <- getErrno
    throwIO (SocketException e)
  return (Socket s)

foreign import ccall safe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt