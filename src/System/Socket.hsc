{-# LANGUAGE TypeFamilies #-}
module System.Socket where

import Control.Exception
import Control.Monad

import Data.Word
import Data.Typeable

import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.ForeignPtr

import GHC.IO
import GHC.Conc
import GHC.Conc.IO

import System.IO
import System.Posix.Types

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
  type SocketAddress f
  familyNumber :: f -> CInt

instance Family AF_INET where
  type SocketAddress AF_INET = SocketAddressInet
  familyNumber _ = (#const AF_INET)

instance Family AF_INET6 where
  type SocketAddress AF_INET6 = SocketAddressInet6
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

newtype CloseException = CloseException Errno
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

instance Show CloseException where
  show (CloseException e@(Errno i))
    | e == eBADF                  = "close: Not a valid open file descriptor."
    | e == eINTR                  = "close: The call was interrupted by a signal."
    | e == eIO                    = "close: An I/O error occurred."
    | otherwise                   = "close: errno " ++ (show i)

instance Exception SocketException
instance Exception CloseException

socket :: (Family f, Type t, Protocol p) => f -> t -> p -> IO (Socket f t p)
socket f t p = do
  s <- c_socket (familyNumber f) (typeNumber t) (protocolNumber p)
  when (s == -1) $ do
    e <- getErrno
    throwIO (SocketException e)
  return (Socket s)


close :: (Family f, Type t, Protocol p) => Socket f t p -> IO ()
close (Socket s) = do
  closeFdWith closeFd (Fd s)
  where
    closeFd (Fd i) = do
      r <- c_close i
      when (r == -1) $ do
        e <- getErrno
        throwIO (CloseException e)

bind :: (Family f, Type t, Protocol p) => Socket f t p -> SocketAddress f -> IO ()
bind = undefined

data SocketAddressStruct

data SocketAddressInet
   = SocketAddressInet
     { sinPort      :: Word16
     , sinAddr      :: Word32
     }

data SocketAddressInet6
   = SocketAddressInet6
     { sin6Port     :: Word16
     , sin6Flowinfo :: Word32
     , sin6Addr_0   :: Word8
     , sin6Addr_1   :: Word8
     , sin6Addr_2   :: Word8
     , sin6Addr_3   :: Word8
     , sin6Addr_4   :: Word8
     , sin6Addr_5   :: Word8
     , sin6Addr_6   :: Word8
     , sin6Addr_7   :: Word8
     , sin6Addr_8   :: Word8
     , sin6Addr_9   :: Word8
     , sin6Addr_A   :: Word8
     , sin6Addr_B   :: Word8
     , sin6Addr_C   :: Word8
     , sin6Addr_D   :: Word8
     , sin6Addr_E   :: Word8
     , sin6Addr_F   :: Word8
     , sin6ScopeId  :: Word32
     }

sockAddrInet6 :: SocketAddressInet6 -> IO (ForeignPtr SocketAddressStruct)
sockAddrInet6 (SocketAddressInet6 p f a_0 a_1 a_2 a_3 a_4 a_5 a_6 a_7 a_8 a_9 a_A a_B a_C a_D a_E a_F s) = do
  fptr <- mallocForeignPtrBytes (#const sizeof(struct sockaddr_storage))
  withForeignPtr fptr (\ptr-> c_sockAddrInet6 ptr p f a_0 a_1 a_2 a_3 a_4 a_5 a_6 a_7 a_8 a_9 a_A a_B a_C a_D a_E a_F s)
  return fptr

foreign import ccall safe "sys/socket.h socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall safe "unistd.h close"
  c_close :: CInt -> IO CInt

foreign import ccall unsafe "misc.h poke_sockaddr_in6"
  c_sockAddrInet6 :: Ptr SocketAddressStruct
                  -> Word16
                  -> Word32
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word8
                  -> Word32
                  -> IO ()
