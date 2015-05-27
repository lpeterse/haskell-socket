{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module System.Socket.Internal (
  -- * Socket
    Socket (..)
  -- * SocketException
  , SocketException (..)
  -- Other Types
    , AddressFamily (..)
  -- *** AF_UNIX
  , AF_UNIX
  , SockAddrUn (..)
  -- *** AF_INET
  , AF_INET
  , SockAddrIn (..)
  -- *** AF_INET6
  , AF_INET6
  , SockAddrIn6 (..)
  -- ** Types
  , Type (..)
  -- *** SOCK_STREAM
  , SOCK_STREAM
  -- *** SOCK_DGRAM
  , SOCK_DGRAM
  -- *** SOCK_SEQPACKET
  , SOCK_SEQPACKET
  -- ** Protocols
  , Protocol  (..)
  -- *** DefaultProtocol
  , DefaultProtocol
  -- *** IPPROTO_UDP
  , IPPROTO_UDP
  -- *** IPPROTO_TCP
  , IPPROTO_TCP
  -- *** IPPROTO_SCTP
  , IPPROTO_SCTP
  -- * FFI
  , c_socket
  , c_bind
  , c_listen
  , c_accept
  , c_connect
  , c_send
  , c_sendto
  , c_recv
  , c_recvfrom
  , c_close
  , c_getsockopt
  , c_setnonblocking

  , threadWaitReadMVar, threadWaitWriteMVar, threadWaitWrite'
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils

import GHC.Conc (threadWaitReadSTM, threadWaitWriteSTM, atomically)

import System.Posix.Types ( Fd(..) )

import System.Socket.Internal.FFI
import System.Socket.Internal.Exception

#include "sys/types.h"
#include "sys/socket.h"
#include "sys/un.h"
#include "netinet/in.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | A generic socket type. Also see `socket` for details.
--
--   The socket is just an `Control.Concurrent.MVar.MVar`-wrapped file descriptor.
--   It is exposed in order to make this library easily extensible, but it is
--   usually not necessary nor advised to work directly on the file descriptor.
--   If you do, the following rules must be obeyed:
--
--   - Make sure not to deadlock. Use `Control.Concurrent.MVar.withMVar` or similar.
--   - The lock __must not__ be held during a blocking call. This would make it impossible
--     to send and receive simultaneously or to close the socket.
--   - The lock __must__ be held when calling operations that use the file descriptor.
--     Otherwise the socket might get closed or even reused by another
--     thread/capability which might result in reading from or writing
--     totally different connection. This is a security nightmare!
--   - The socket is non-blocking and all the code relies on that assumption.
--     You need to use GHC's eventing mechanism primitives to block until
--     something happens. The former rules forbid to use `GHC.Conc.threadWaitRead` as it
--     does not seperate between registering the file descriptor (for which
--     the lock __must__ be held) and the actual waiting (for which you must
--     __not__ hold the lock).
--     Also see [this](https://mail.haskell.org/pipermail/haskell-cafe/2014-September/115823.html)
--     thread and read the library code to see how the problem is currently circumvented.
newtype Socket d t p
      = Socket (MVar Fd)

data AF_UNIX
data AF_INET
data AF_INET6

data SOCK_STREAM
data SOCK_DGRAM
data SOCK_SEQPACKET

data DefaultProtocol
data IPPROTO_UDP
data IPPROTO_TCP
data IPPROTO_SCTP

class (Storable (SockAddr f)) => AddressFamily f where
  type SockAddr f
  addressFamilyNumber :: f -> CInt

instance AddressFamily AF_UNIX where
  type SockAddr AF_UNIX = SockAddrUn
  addressFamilyNumber _ = (#const AF_UNIX)

instance AddressFamily AF_INET where
  type SockAddr AF_INET = SockAddrIn
  addressFamilyNumber _ = (#const AF_INET)

instance AddressFamily AF_INET6 where
  type SockAddr AF_INET6 = SockAddrIn6
  addressFamilyNumber _ = (#const AF_INET6)

class Type t where
  typeNumber :: t -> CInt

instance Type SOCK_STREAM where
  typeNumber _ = (#const SOCK_STREAM)

instance Type SOCK_DGRAM where
  typeNumber _ = (#const SOCK_DGRAM)

instance Type SOCK_SEQPACKET where
  typeNumber _ = (#const SOCK_SEQPACKET)

class Protocol  p where
  protocolNumber :: p -> CInt

instance Protocol DefaultProtocol where
  protocolNumber _ = 0

instance Protocol  IPPROTO_TCP where
  protocolNumber _ = (#const IPPROTO_TCP)

instance Protocol  IPPROTO_UDP where
  protocolNumber _ = (#const IPPROTO_UDP)

instance Protocol  IPPROTO_SCTP where
  protocolNumber _ = (#const IPPROTO_SCTP)

data SockAddrUn
   = SockAddrUn
     { sunPath      :: BS.ByteString
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

instance Storable SockAddrUn where
  sizeOf    _ = (#size struct sockaddr_un)
  alignment _ = (#alignment struct sockaddr_un)
  peek ptr    = do
    path <- BS.packCString (sun_path ptr) :: IO BS.ByteString
    return (SockAddrUn path)
    where
      sun_path = (#ptr struct sockaddr_un, sun_path)
  poke ptr (SockAddrUn path) = do
    -- useAsCString null-terminates the CString
    BS.useAsCString truncatedPath $ \cs-> do
      copyBytes (sun_path ptr) cs (BS.length truncatedPath + 1)-- copyBytes dest from count
    where
      sun_path      = (#ptr struct sockaddr_un, sun_path)
      truncatedPath = BS.take ( sizeOf (undefined :: SockAddrUn)
                              - sizeOf (undefined :: Word16)
                              - 1 ) path

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

-------------------------------------------------------------------------------
-- Helpers for threadsafe event registration on file descriptors
-------------------------------------------------------------------------------

threadWaitReadMVar :: MVar Fd -> IO ()
threadWaitReadMVar mfd = do
  wait <- withMVar mfd $ \fd-> do
    when (fd < 0) $ throwIO (SocketException eBADF)
    threadWaitReadSTM fd >>= return . atomically . fst
  wait `onException` throwIO (SocketException eBADF)

threadWaitWriteMVar :: MVar Fd -> IO ()
threadWaitWriteMVar mfd = do
  wait <- withMVar mfd $ \fd-> do
    when (fd < 0) $ throwIO (SocketException eBADF)
    threadWaitWriteSTM fd >>= return . atomically . fst
  wait `onException` throwIO (SocketException eBADF)

threadWaitWrite' :: Fd -> IO (IO ())
threadWaitWrite' fd = do
  threadWaitWriteSTM fd >>= return . atomically . fst
