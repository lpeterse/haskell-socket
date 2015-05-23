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
   -- ** send / sendTo
   , send, sendTo
   -- ** recv / recvFrom
   , recv, recvFrom
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

  , localhost
  ) where

import Control.Exception
import Control.Monad
import Control.Concurrent.MVar

import Data.Word
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import GHC.Conc (threadWaitReadSTM, threadWaitWriteSTM, atomically, closeFdWith)

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
      = Socket (MVar Fd)

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

socket :: (SocketDomain d, SocketType t, SocketProtocol  p) => IO (Socket d t p)
socket = socket'
 where
   socket' :: forall d t p. (SocketDomain d, SocketType t, SocketProtocol  p) => IO (Socket d t p)
   socket'  = do
     bracketOnError
       -- Try to acquire the socket resource. This part has exceptions masked.
       ( c_socket (socketDomain (undefined :: d)) (typeNumber (undefined :: t)) (protocolNumber (undefined :: p)) )
       -- On failure after the c_socket call we try to close the socket to not leak file descriptors.
       -- If closing fails we cannot really do something about it. We tried at least.
       -- This part has exceptions masked as well. c_close is an unsafe FFI call.
       ( \fd-> when (fd >= 0) (c_close fd >> return ()) )
       -- If an exception is raised, it is reraised after the socket has been closed.
       -- This part has async exceptions unmasked (via restore).
       ( \fd-> if fd < 0 then do
                getErrno >>= throwIO . SocketException
              else do
                -- setNonBlockingFD calls c_fcntl_write which is an unsafe FFI call.
                let Fd s = fd in setNonBlockingFD s True
                mfd <- newMVar fd
                return (Socket mfd)
       )

-- | Closes a socket.
-- In contrast to the POSIX close this operation is idempotent.
-- On EINTR the close operation is retried.
-- On EBADF an error is thrown as this should be impossible according to the library's design.
-- On EIO an error is thrown.
close :: (SocketDomain d, SocketType t, SocketProtocol  p) => Socket d t p -> IO ()
close (Socket mfd) = do
  modifyMVarMasked_ mfd $ \fd-> do
    if fd < 0 then do
      return fd
    else do
      -- closeFdWith does not throw even on invalid file descriptors.
      -- It just assures no thread is blocking on the fd anymore and then executes the IO action.
      closeFdWith closeLoop fd
      -- When we arrive here, no exception has been thrown and the descriptor has been closed.
      -- We put an invalid file descriptor into the MVar.
      return (-1)
  where
    -- The c_close operation may (according to Posix documentation) fails with EINTR or EBADF or EIO.
    -- EBADF: Should be ruled out by the library's design.
    -- EINTR: It is best practice to just retry the operation what we do here.
    -- EIO: Only occurs when filesystem is involved (?).
    -- Conclusion: Our close should never fail. If it does, something is horribly wrong.
    closeLoop fd = do
      i <- c_close fd
      if i < 0 then do
        e <- getErrno
        if e == eINTR 
          then closeLoop fd
          else throwIO (SocketException e)
      else return ()

bind :: (SocketDomain d, SocketType t, SocketProtocol  p) => Socket d t p -> SocketAddress d -> IO ()
bind (Socket ms) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    i <- withMVar ms $ \s-> do
      c_bind s (castPtr addrPtr :: Ptr ()) (sizeOf addr)
    if i < 0 then do
      getErrno >>= throwIO . SocketException
    else do
      return ()

connect :: (SocketDomain d, SocketType t, SocketProtocol  p) => Socket d t p -> SocketAddress d -> IO ()
connect (Socket ms) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    i <- withMVar ms $ \s-> do
      c_connect s (castPtr addrPtr :: Ptr ()) (sizeOf addr)
    if i < 0 then do
      getErrno >>= throwIO . SocketException
    else do
      return ()

accept :: (SocketDomain d, SocketType t, SocketProtocol  p) => Socket d t p -> IO (Socket d t p, SocketAddress d)
accept = accept'
  where
    accept' :: forall d t p. (SocketDomain d, SocketType t, SocketProtocol  p) => Socket d t p -> IO (Socket d t p, SocketAddress d)
    accept' (Socket mfd) = do
      -- Block until notification about read.
      -- IOError is thrown if the socket has been closed in the meantime.
      -- We reraise this as a SocketError.
      threadWaitReadMVar mfd `onException` throwIO (SocketException eBADF)
      -- Accept uses the socket exclusively by acquiring the MVar.
      withMVar mfd $ \fd-> do
        -- If the socket has already been closed we fail excactly like the accept call would fail.
        when (fd < 0) $ do
          throwIO (SocketException eBADF)
        -- Allocate local (!) memory for the address.
        alloca $ \addrPtr-> do
          bracketOnError
            ( c_accept fd (castPtr addrPtr :: Ptr ()) (sizeOf (undefined :: SocketAddress d)) )
            ( \ft-> when (fd >= 0) (c_close ft >> return ()) )
            ( \ft-> if ft < 0 then do
                     getErrno >>= throwIO . SocketException
                   else do
                     -- setNonBlockingFD calls c_fcntl_write which is an unsafe FFI call.
                     let Fd i = ft in setNonBlockingFD i True
                     addr <- peek addrPtr :: IO (SocketAddress d)
                     mft <- newMVar ft
                     return (Socket mft, addr)
            )

listen :: (SocketDomain d, SocketType t, SocketProtocol  p) => Socket d t p -> Int -> IO ()
listen (Socket ms) backlog = do
  i <- withMVar ms $ \s-> do
    c_listen s backlog
  if i < 0 then do
    getErrno >>= throwIO . SocketException
  else do
    return ()

recv     :: Socket d t p -> Int -> IO BS.ByteString
recv = undefined

recvFrom :: Socket d t p -> Int -> IO (BS.ByteString, SocketAddress d)
recvFrom = undefined

send     :: Socket d t p -> BS.ByteString -> IO Int
send (Socket mfd) bs = sendWait
  where
    sendWait = do
      threadWaitWriteMVar mfd
      bytesSend <- withMVar mfd $ \fd-> do
        when (fd < 0) $ do
          throwIO (SocketException eBADF)
        BS.unsafeUseAsCStringLen bs $ \(ptr,len)->
          let loop = do
                i <- c_send fd ptr len 0
                if (i < 0) then do
                  e <- getErrno
                  if e == eWOULDBLOCK || e == eAGAIN 
                    then return i
                  else if e == eINTR
                    then loop
                    else throwIO (SocketException e)
                -- Send succeeded. Return the bytes send.
                else return i
          in loop
      -- We cannot loop from within the block above, because this would keep the MVar locked.
      if bytesSend < 0
        then sendWait
        else return bytesSend

sendTo   :: Socket d t p -> BS.ByteString -> SocketAddress d -> IO Int
sendTo = undefined

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
  c_socket  :: CInt -> CInt -> CInt -> IO Fd

foreign import ccall safe "unistd.h close"
  c_close   :: Fd -> IO CInt

foreign import ccall safe "sys/socket.h bind"
  c_bind    :: Fd -> Ptr () -> Int -> IO CInt

foreign import ccall safe "sys/socket.h connect"
  c_connect :: Fd -> Ptr () -> Int -> IO CInt

foreign import ccall safe "sys/socket.h accept"
  c_accept  :: Fd -> Ptr () -> Int -> IO Fd

foreign import ccall safe "sys/socket.h listen"
  c_listen  :: Fd -> Int -> IO CInt

foreign import ccall safe "sys/socket.h send"
  c_send  :: Fd -> Ptr CChar -> Int -> Int -> IO Int

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

-- helpers for threadsafe event registration on file descriptors

threadWaitReadMVar :: MVar Fd -> IO ()
threadWaitReadMVar mfd = do
  withMVar mfd threadWaitWriteSTM >>= atomically . fst

threadWaitWriteMVar :: MVar Fd -> IO ()
threadWaitWriteMVar mfd = do
  withMVar mfd threadWaitWriteSTM >>=  atomically . fst