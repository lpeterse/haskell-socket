{-# LANGUAGE TypeFamilies #-}
module System.Socket where

import Control.Exception
import Control.Monad

import Data.Word
import Data.Bits
import Data.Typeable

import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

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
     { sin6Port      :: Word16
     , sin6Flowinfo  :: Word32
     , sin6Addr_high :: Word64
     , sin6Addr_low  :: Word64
     , sin6ScopeId   :: Word32
     }

foreign import ccall safe "sys/socket.h socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall safe "unistd.h close"
  c_close :: CInt -> IO CInt

foreign import ccall unsafe "misc.h poke_sockaddr_in6"
  c_poke_sockaddr_in6 :: Ptr SocketAddressInet6
    -> Word16
    -> Word32
    -> Word64
    -> Word64
    -> Word32
    -> IO ()

instance Storable SocketAddressInet6 where
  sizeOf    _ = (#const sizeof(struct sockaddr_storage))
  alignment _ = 8
  peek ptr    = do
    f      <- (#peek struct sockaddr_in6, sin6_flowinfo) ptr               :: IO Word32
    s      <- (#peek struct sockaddr_in6, sin6_scope_id) ptr               :: IO Word32
    p_high <- peek $ (#ptr struct sockaddr_in6, sin6_port) ptr             :: IO Word8
    p_low  <- peek $ (#ptr struct sockaddr_in6, sin6_port) ptr `plusPtr` 1 :: IO Word8

    let p = (fromIntegral p_high) * 256 + fromIntegral p_low               :: Word16
    let addrPtr = (#ptr struct in6_addr, s6_addr) ((#ptr struct sockaddr_in6, sin6_addr) ptr)

    b00 <- peekByteOff addrPtr  0 :: IO Word8
    b01 <- peekByteOff addrPtr  1 :: IO Word8
    b02 <- peekByteOff addrPtr  2 :: IO Word8
    b03 <- peekByteOff addrPtr  3 :: IO Word8
    b04 <- peekByteOff addrPtr  4 :: IO Word8
    b05 <- peekByteOff addrPtr  5 :: IO Word8
    b06 <- peekByteOff addrPtr  6 :: IO Word8
    b07 <- peekByteOff addrPtr  7 :: IO Word8
    b08 <- peekByteOff addrPtr  8 :: IO Word8
    b09 <- peekByteOff addrPtr  9 :: IO Word8
    b10 <- peekByteOff addrPtr 10 :: IO Word8
    b11 <- peekByteOff addrPtr 11 :: IO Word8
    b12 <- peekByteOff addrPtr 12 :: IO Word8
    b13 <- peekByteOff addrPtr 13 :: IO Word8
    b14 <- peekByteOff addrPtr 14 :: IO Word8
    b15 <- peekByteOff addrPtr 15 :: IO Word8

    let a_high = fromIntegral b00 * 256 * 256 * 256 * 256 * 256 * 256 * 256
               + fromIntegral b01 * 256 * 256 * 256 * 256 * 256 * 256
               + fromIntegral b02 * 256 * 256 * 256 * 256 * 256
               + fromIntegral b03 * 256 * 256 * 256 * 256
               + fromIntegral b04 * 256 * 256 * 256
               + fromIntegral b05 * 256 * 256
               + fromIntegral b06 * 256
               + fromIntegral b07 :: Word64

    let a_low  = fromIntegral b08 * 256 * 256 * 256 * 256 * 256 * 256 * 256
               + fromIntegral b09 * 256 * 256 * 256 * 256 * 256 * 256
               + fromIntegral b10 * 256 * 256 * 256 * 256 * 256
               + fromIntegral b11 * 256 * 256 * 256 * 256
               + fromIntegral b12 * 256 * 256 * 256
               + fromIntegral b13 * 256 * 256
               + fromIntegral b14 * 256
               + fromIntegral b15 :: Word64

    return (SocketAddressInet6 p f a_high a_low s)

  poke ptr (SocketAddressInet6 p f a_high a_low s) = do
    poke        (sin6_family ptr) ((#const AF_INET) :: Word16)
    poke        (sin6_flowinfo ptr) f
    poke        (sin6_scope_id ptr) s
    pokeByteOff (sin6_port ptr)  0 (fromIntegral $ 0xff .&. (p      `shiftR`  8) :: Word8)
    pokeByteOff (sin6_port ptr)  1 (fromIntegral $ 0xff .&. (p      `shiftR`  0) :: Word8)
    pokeByteOff (sin6_addr ptr)  0 (fromIntegral $ 0xff .&. (a_high `shiftR` 56) :: Word8)
    pokeByteOff (sin6_addr ptr)  1 (fromIntegral $ 0xff .&. (a_high `shiftR` 48) :: Word8)
    pokeByteOff (sin6_addr ptr)  2 (fromIntegral $ 0xff .&. (a_high `shiftR` 40) :: Word8)
    pokeByteOff (sin6_addr ptr)  3 (fromIntegral $ 0xff .&. (a_high `shiftR` 32) :: Word8)
    pokeByteOff (sin6_addr ptr)  4 (fromIntegral $ 0xff .&. (a_high `shiftR` 24) :: Word8)
    pokeByteOff (sin6_addr ptr)  5 (fromIntegral $ 0xff .&. (a_high `shiftR` 16) :: Word8)
    pokeByteOff (sin6_addr ptr)  6 (fromIntegral $ 0xff .&. (a_high `shiftR`  8) :: Word8)
    pokeByteOff (sin6_addr ptr)  7 (fromIntegral $ 0xff .&. (a_high `shiftR`  0) :: Word8)
    pokeByteOff (sin6_addr ptr)  8 (fromIntegral $ 0xff .&. (a_high `shiftR` 56) :: Word8)
    pokeByteOff (sin6_addr ptr)  9 (fromIntegral $ 0xff .&. (a_high `shiftR` 48) :: Word8)
    pokeByteOff (sin6_addr ptr) 10 (fromIntegral $ 0xff .&. (a_high `shiftR` 40) :: Word8)
    pokeByteOff (sin6_addr ptr) 11 (fromIntegral $ 0xff .&. (a_high `shiftR` 32) :: Word8)
    pokeByteOff (sin6_addr ptr) 12 (fromIntegral $ 0xff .&. (a_high `shiftR` 24) :: Word8)
    pokeByteOff (sin6_addr ptr) 13 (fromIntegral $ 0xff .&. (a_high `shiftR` 16) :: Word8)
    pokeByteOff (sin6_addr ptr) 14 (fromIntegral $ 0xff .&. (a_high `shiftR`  8) :: Word8)
    pokeByteOff (sin6_addr ptr) 15 (fromIntegral $ 0xff .&. (a_high `shiftR`  0) :: Word8)
    where
      sin6_family   = (#ptr struct sockaddr_in6, sin6_family)
      sin6_flowinfo = (#ptr struct sockaddr_in6, sin6_flowinfo)
      sin6_scope_id = (#ptr struct sockaddr_in6, sin6_scope_id)
      sin6_port     = (#ptr struct sockaddr_in6, sin6_port)
      sin6_addr     = (#ptr struct in6_addr, s6_addr) . (#ptr struct sockaddr_in6, sin6_addr)