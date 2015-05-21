{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module System.Socket
  ( -- * Operations
   -- ** socket
     socket
   -- ** bind
   , bind
   -- ** listen
   , listen
   -- ** accept
   , accept
   -- ** connect
   , connect
   -- ** close
   , close

  -- * Sockets
  , Socket (..)
  -- ** Domains
  , SocketDomain (..)
  -- *** AF_UNIX
  , AF_UNIX
  , SockAddrUn
  -- *** AF_INET
  , AF_INET
  , SockAddrIn (..)
  -- *** AF_INET6
  , AF_INET6
  , SockAddrIn6 (..)
  -- ** Types
  , SocketType (..)
  -- *** SOCK_STREAM
  , SOCK_STREAM
  -- *** SOCK_DGRAM
  , SOCK_DGRAM
  -- *** SOCK_SEQPACKET
  , SOCK_SEQPACKET
  -- ** Protocols
  , SocketProtocol  (..)
  -- *** IPPROTO_UDP
  , IPPROTO_UDP
  -- *** IPPROTO_TCP
  , IPPROTO_TCP
  ) where

import Control.Exception
import Control.Monad
import Control.Concurrent.MVar

import Data.Word
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils

import System.IO
import System.Posix.Types
import System.Posix.Internals (setNonBlockingFD)

#include "sys/types.h"
#include "sys/socket.h"
#include "netinet/in.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

newtype Socket d t p
      = Socket (MVar CInt)

data AF_UNIX
data AF_INET
data AF_INET6

data SOCK_STREAM
data SOCK_DGRAM
data SOCK_SEQPACKET

data IPPROTO_UDP
data IPPROTO_TCP

data IP_SOCKOPT
   = IP_MTU
   | IP_TTL

data IP6_SOCKOPT

data TCP_SOCKOPT
   = TCP_CONGESTION
   | TCP_CORK
   | TCP_DEFER_ACCEPT
   | TCP_INFO
   | TCP_KEEPCNT
   | TCP_KEEPIDLE
   | TCP_KEEPINTVL
   | TCP_MAXSEG
   | TCP_NODELAY
   | TCP_QUICKACK
   | TCP_SYNCNT
   | TCP_USER_TIMEOUT
   | TCP_WINDOW_CLAMP

setSockOptSocketDomain :: (SocketDomain f, SocketType t, SocketProtocol  p) => Socket f t p -> SockOpt f -> IO ()
setSockOptSocketDomain
  = undefined

setSockOptSocketProtocol  :: (SocketDomain f, SocketType t, SocketProtocol  p) => Socket f t p -> SockOpt p -> IO ()
setSockOptSocketProtocol 
  = undefined

class (Storable (SocketAddress d)) => SocketDomain d where
  type SocketAddress d
  socketDomain :: d -> CInt

type family SockOpt o :: *
type instance SockOpt AF_INET     = IP_SOCKOPT
type instance SockOpt AF_INET6    = IP6_SOCKOPT
type instance SockOpt IPPROTO_TCP = TCP_SOCKOPT

instance SocketDomain AF_UNIX where
  type SocketAddress AF_UNIX = SockAddrUn
  socketDomain _ = (#const AF_UNIX)

instance SocketDomain AF_INET where
  type SocketAddress AF_INET = SockAddrIn
  socketDomain _ = (#const AF_INET)

instance SocketDomain AF_INET6 where
  type SocketAddress AF_INET6 = SockAddrIn6
  socketDomain _ = (#const AF_INET6)

class SocketType t where
  typeNumber :: t -> CInt

instance SocketType SOCK_STREAM where
  typeNumber _ = (#const SOCK_STREAM)

instance SocketType SOCK_DGRAM where
  typeNumber _ = (#const SOCK_DGRAM)

instance SocketType SOCK_SEQPACKET where
  typeNumber _ = (#const SOCK_SEQPACKET)

class SocketProtocol  p where
  protocolNumber :: p -> CInt

instance SocketProtocol  IPPROTO_TCP where
  protocolNumber _ = (#const IPPROTO_TCP)

newtype SocketException = SocketException Errno
  deriving Typeable

instance Show SocketException where
  show (SocketException (Errno e)) = "SocketException (errno " ++ show e ++ ")"

instance Exception SocketException

socket :: forall f t p. (SocketDomain f, SocketType t, SocketProtocol  p) => IO (Socket f t p)
socket = do
  bracketOnError
    -- Try to acquire the socket resource. This part has exceptions masked.
    ( c_socket (socketDomain (undefined :: f)) (typeNumber (undefined :: t)) (protocolNumber (undefined :: p)) )
    -- On failure after the c_socket call we try to close the socket to not leak file descriptors.
    -- If closing fails we cannot really do something about it. We tried at least.
    -- This part has exceptions masked as well. c_close is an unsafe FFI call.
    ( \s-> when (s >= 0) (c_close s >> return ()) )
    -- If an exception is raised, it is reraised after the socket has been closed.
    -- This part has async exceptions unmasked (via restore).
    ( \s-> if s < 0 then do
             getErrno >>= throwIO . SocketException
           else do
             -- setNonBlockingFD calls c_fcntl_write which is an unsafe FFI call.
             setNonBlockingFD s True
             ms <- newMVar s
             return (Socket ms)
    )

-- | Closes a socket.
-- In contrast to the POSIX close this operation is idempotent.
-- On EINTR the close operation is retried.
-- On EBADF an error is thrown as this should be impossible according to the library's design.
-- On EIO an error is thrown.
close :: (SocketDomain f, SocketType t, SocketProtocol  p) => Socket f t p -> IO ()
close (Socket ms) = do
  failure <- modifyMVarMasked ms $ \s-> do
    -- The socket has already been closed.
    if s == closed then do
      return (closed, False)
    -- Close the socket via system call.
    else do
      i <- c_close s
      -- The close call failed. Preserve socket descriptor to allow a retry.
      if i < 0 then
        return (s, True)
      -- The close call succeeded. Note this with a -1 as socket descriptor.
      else do
        return (closed, False)
  if failure then do
    getErrno >>= throwIO . SocketException
  else do
    return ()
  where
    closed = -1

bind :: (SocketDomain f, SocketType t, SocketProtocol  p) => Socket f t p -> SocketAddress f -> IO ()
bind (Socket ms) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    i <- withMVar ms $ \s-> do
      c_bind s (castPtr addrPtr :: Ptr ()) (sizeOf addr)
    if i < 0 then do
      getErrno >>= throwIO . SocketException
    else do
      return ()

connect :: (SocketDomain f, SocketType t, SocketProtocol  p) => Socket f t p -> SocketAddress f -> IO ()
connect (Socket ms) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    i <- withMVar ms $ \s-> do
      c_connect s (castPtr addrPtr :: Ptr ()) (sizeOf addr)
    if i < 0 then do
      getErrno >>= throwIO . SocketException
    else do
      return ()

accept :: forall f t p. (SocketDomain f, SocketType t, SocketProtocol  p) => Socket f t p -> IO (Socket f t p, SocketAddress f)
accept (Socket ms) = do
  alloca $ \addrPtr-> do
    i <- withMVar ms $ \s-> do
      c_accept s (castPtr addrPtr :: Ptr ()) (sizeOf (undefined :: SocketAddress f))
    if i < 0 then do
      getErrno >>= throwIO . SocketException
    else do
      addr <- peek addrPtr
      ms'  <- newMVar i
      return (Socket ms', addr)

listen :: forall f t p. (SocketDomain f, SocketType t, SocketProtocol  p) => Socket f t p -> Int -> IO ()
listen (Socket ms) backlog = do
  i <- withMVar ms $ \s-> do
    c_listen s backlog
  if i < 0 then do
    getErrno >>= throwIO . SocketException
  else do
    return ()

data SockAddrUn
   = SockAddrUn
     {

     } deriving (Eq, Ord, Show)

data SockAddrIn
   = SockAddrIn
     { sinPort      :: Word16
     , sinAddr      :: BS.ByteString
     } deriving (Eq, Ord, Show)

data SockAddrIn6
   = SockAddrIn6
     { sin6Port      :: Word16
     , sin6Flowinfo  :: Word32
     , sin6Addr      :: BS.ByteString
     , sin6ScopeId   :: Word32
     } deriving (Eq, Ord, Show)

localhost :: SockAddrIn6
localhost =
  SockAddrIn6
  { sin6Port     = 5555
  , sin6Flowinfo = 0
  , sin6Addr     = BS.pack [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1]
  , sin6ScopeId  = 0
  }

foreign import ccall safe "sys/socket.h socket"
  c_socket  :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall safe "unistd.h close"
  c_close   :: CInt -> IO CInt

foreign import ccall safe "sys/socket.h bind"
  c_bind    :: CInt -> Ptr () -> Int -> IO CInt

foreign import ccall safe "sys/socket.h connect"
  c_connect :: CInt -> Ptr () -> Int -> IO CInt

foreign import ccall safe "sys/socket.h accept"
  c_accept  :: CInt -> Ptr () -> Int -> IO CInt

foreign import ccall safe "sys/socket.h listen"
  c_listen  :: CInt -> Int -> IO CInt

instance Storable SockAddrUn where
  sizeOf = undefined
  alignment = undefined
  peek = undefined
  poke = undefined

instance Storable SockAddrIn where
  sizeOf    _ = (#size struct sockaddr_in)
  alignment _ = (#alignment struct sockaddr_in)
  peek ptr    = do
    ph  <- peekByteOff       (sin_port ptr)  0  :: IO Word8
    pl  <- peekByteOff       (sin_port ptr)  1  :: IO Word8
    a   <- BS.packCStringLen (sin_addr ptr,  4) :: IO BS.ByteString
    return (SockAddrIn (fromIntegral ph * 256 + fromIntegral pl) a)
    where
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
  poke ptr (SockAddrIn p a) = do
    poke        (sin_family   ptr) ((#const AF_INET) :: Word16)
    pokeByteOff (sin_port     ptr)  0 (fromIntegral $ rem (quot p 256) 256 :: Word8)
    pokeByteOff (sin_port     ptr)  1 (fromIntegral $ rem       p      256 :: Word8)
    BS.unsafeUseAsCString a $ \a'-> do
      copyBytes (sin_addr ptr) a' (min 4 $ BS.length a)-- copyBytes dest from count
    where
      sin_family   = (#ptr struct sockaddr_in, sin_family)
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)

instance Storable SockAddrIn6 where
  sizeOf    _ = (#size struct sockaddr_in6)
  alignment _ = (#alignment struct sockaddr_in6)
  peek ptr    = do
    f   <- peek              (sin6_flowinfo ptr) :: IO Word32
    ph  <- peekByteOff       (sin6_port ptr)  0  :: IO Word8
    pl  <- peekByteOff       (sin6_port ptr)  1  :: IO Word8
    a   <- BS.packCStringLen (sin6_addr ptr, 16) :: IO BS.ByteString
    s   <- peek              (sin6_scope_id ptr) :: IO Word32
    return (SockAddrIn6 (fromIntegral ph * 256 + fromIntegral pl) f a s)
    where
      sin6_flowinfo = (#ptr struct sockaddr_in6, sin6_flowinfo)
      sin6_scope_id = (#ptr struct sockaddr_in6, sin6_scope_id)
      sin6_port     = (#ptr struct sockaddr_in6, sin6_port)
      sin6_addr     = (#ptr struct in6_addr, s6_addr) . (#ptr struct sockaddr_in6, sin6_addr)
  poke ptr (SockAddrIn6 p f a s) = do
    poke        (sin6_family   ptr) ((#const AF_INET6) :: Word16)
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