{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module System.Socket where

import Control.Exception
import Control.Monad

import Data.Word
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Utils

import GHC.IO
import GHC.Conc
import GHC.Conc.IO

import System.IO
import System.Posix.Types

#include "sys/types.h"
#include "sys/socket.h"
#include "netinet/in.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

newtype Socket f t p
      = Socket CInt

data AF_INET
data AF_INET6

data SOCK_STREAM
data SOCK_DGRAM
data SOCK_SEQPACKET

data IPPROTO_UDP
data IPPROTO_TCP
data IPPROTO_SCTP

class (Storable (Address f)) => Family f where
  type Address f
  familyNumber :: f -> CInt

instance Family AF_INET where
  type Address AF_INET = SocketAddressInet
  familyNumber _ = (#const AF_INET)

instance Family AF_INET6 where
  type Address AF_INET6 = SocketAddressInet6
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

socket :: forall f t p. (Family f, Type t, Protocol p) => IO (Either Errno (Socket f t p))
socket = do
  s <- c_socket (familyNumber (undefined :: f)) (typeNumber (undefined :: t)) (protocolNumber (undefined :: p))
  if s == -1 then do
    e <- getErrno
    return (Left e)
  else do
    return (Right (Socket s))

close :: (Family f, Type t, Protocol p) => Socket f t p -> IO (Either Errno ())
close (Socket s) = do
  r <- c_close s
  if r == -1 then do
    e <- getErrno
    return (Left e)
  else do
    return (Right ())

bind :: (Family f, Type t, Protocol p) => Socket f t p -> Address f -> IO (Either Errno ())
bind (Socket s) addr = do
  addrForeignPtr <- mallocForeignPtr
  withForeignPtr addrForeignPtr $ \addrPtr-> do
    poke addrPtr addr
    r <- c_bind s (castPtr addrPtr :: Ptr SocketAddress) (sizeOf addr)
    if r == -1 then do
      e <- getErrno
      return (Left e)
    else do
      return (Right ())

data SocketAddress

data SocketAddressInet
   = SocketAddressInet
     { sinPort      :: Word16
     , sinAddr      :: BS.ByteString
     }

data SocketAddressInet6
   = SocketAddressInet6
     { sin6Port      :: Word16
     , sin6Flowinfo  :: Word32
     , sin6Addr      :: BS.ByteString
     , sin6ScopeId   :: Word32
     }

foreign import ccall safe "sys/socket.h socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall safe "unistd.h close"
  c_close :: CInt -> IO CInt

foreign import ccall safe "sys/socket.h bind"
  c_bind :: CInt -> Ptr SocketAddress -> Int -> IO CInt

instance Storable SocketAddressInet where
  sizeOf    _ = (#size struct sockaddr_in)
  alignment _ = 8
  peek        = undefined
  poke        = undefined

instance Storable SocketAddressInet6 where
  sizeOf    _ = (#size struct sockaddr_in6)
  alignment _ = (#alignment struct sockaddr_in6)
  peek ptr    = do
    f   <- peek              (sin6_flowinfo ptr) :: IO Word32
    ph  <- peekByteOff       (sin6_port ptr)  0  :: IO Word8
    pl  <- peekByteOff       (sin6_port ptr)  1  :: IO Word8
    a   <- BS.packCStringLen (sin6_addr ptr, 16) :: IO BS.ByteString
    s   <- peek              (sin6_scope_id ptr) :: IO Word32
    return (SocketAddressInet6 (fromIntegral ph * 256 + fromIntegral pl) f a s)
    where
      sin6_family   = (#ptr struct sockaddr_in6, sin6_family)
      sin6_flowinfo = (#ptr struct sockaddr_in6, sin6_flowinfo)
      sin6_scope_id = (#ptr struct sockaddr_in6, sin6_scope_id)
      sin6_port     = (#ptr struct sockaddr_in6, sin6_port)
      sin6_addr     = (#ptr struct in6_addr, s6_addr) . (#ptr struct sockaddr_in6, sin6_addr)
  poke ptr (SocketAddressInet6 p f a s) = do
    poke        (sin6_family   ptr) ((#const AF_INET) :: Word16)
    poke        (sin6_flowinfo ptr) f
    poke        (sin6_scope_id ptr) s
    pokeByteOff (sin6_port     ptr)  0 (fromIntegral $ rem (quot p 256) 256 :: Word8)
    pokeByteOff (sin6_port     ptr)  1 (fromIntegral $ rem       p      256 :: Word8)
    BS.unsafeUseAsCString a $ \a'-> do
      copyBytes (sin6_addr ptr) a' (min 16 $ BS.length a)-- copyBytes dest from count
    where
      sin6_family   = (#ptr struct sockaddr_in6, sin6_family)
      sin6_flowinfo = (#ptr struct sockaddr_in6, sin6_flowinfo)
      sin6_scope_id = (#ptr struct sockaddr_in6, sin6_scope_id)
      sin6_port     = (#ptr struct sockaddr_in6, sin6_port)
      sin6_addr     = (#ptr struct in6_addr, s6_addr) . (#ptr struct sockaddr_in6, sin6_addr)