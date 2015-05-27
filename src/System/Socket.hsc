{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables,
             DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Socket
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import System.Socket
-- > import Data.ByteString
-- > import Control.Monad
-- > import Control.Concurrent
-- >
-- > main :: IO ()
-- > main = do
-- >   s <- socket :: IO (Socket AF_INET SOCK_STREAM IPPROTO_TCP)
-- >   bind s (SockAddrIn 8080 (pack [127,0,0,1]))
-- >   listen s 5
-- >   forever $ do
-- >     (peer,addr) <- accept s
-- >     forkIO $ do
-- >       send peer "Hello world!"
-- >       close peer
-----------------------------------------------------------------------------
module System.Socket (
  -- * Operations
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
  -- ** send
  , send
  -- ** sendTo
  , sendTo
  -- ** recv
  , recv
  -- ** recvFrom
  , recvFrom
  -- ** close
  , close

  -- * Sockets
  , Socket (..)
  -- ** Address Families
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

  , SocketException (..)

  -- * Options
  -- ** setSockOpt, getSockOpt
  , GetSockOpt (..)
  , SetSockOpt (..)
  -- ** SO_ACCEPTCONN
  , SO_ACCEPTCONN (..)
  ) where

import Control.Exception
import Control.Monad
import Control.Concurrent.MVar

import Data.Function
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

import System.Posix.Types
import System.Posix.Internals (setNonBlockingFD)

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

-- | Creates a new socket.
--
--   Whereas the underlying POSIX socket function takes 3 parameters, this library
--   encodes this information in the type variables. This rules out several
--   kinds of errors and escpecially simplifies the handling of addresses (by using
--   associated type families). Examples:
--
--   > -- create a IPv4-UDP-datagram socket
--   > sock <- socket :: IO (Socket AF_INET SOCK_DGRAM IPPROTO_UDP)
--   > -- create a IPv6-TCP-streaming socket
--   > sock6 <- socket :: IO (Socket AF_INET6 SOCK_STREAM IPPROTO_TCP)
--
--     - This operation sets up a finalizer that automatically closes the socket
--       when the garbage collection decides to collect it. This is just a
--       fail-safe. You might still run out of file descriptors as there's
--       no guarantee about when the finalizer is run. You're advised to
--       manually `close` the socket when it's no longer needed.
--     - This operation configures the socket non-blocking to work seamlessly
--       with the runtime system's event notification mechanism.
--     - This operation can safely deal with asynchronous exceptions without
--       leaking file descriptors.
--     - This operation throws `SocketException`s:
--
--        [@EAFNOSUPPORT@]    The socket domain is not supported.
--        [@EMFILE@]          The process is out file descriptors.
--        [@ENFILE@]          The system is out file descriptors.
--        [@EPROTONOSUPPORT@] The socket protocol is not supported (for this socket domain).
--        [@EPROTOTYPE@]      The socket type is not supported by the protocol.
--        [@EACCES@]          The process is lacking necessary privileges.
--        [@ENOMEM@]          Insufficient memory.
socket :: (AddressFamily f, Type t, Protocol  p) => IO (Socket f t p)
socket = socket'
 where
   socket' :: forall f t p. (AddressFamily f, Type t, Protocol  p) => IO (Socket f t p)
   socket'  = do
     bracketOnError
       -- Try to acquire the socket resource. This part has exceptions masked.
       ( c_socket (addressFamilyNumber (undefined :: f)) (typeNumber (undefined :: t)) (protocolNumber (undefined :: p)) )
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
                let s = Socket mfd
                _ <- mkWeakMVar mfd (close s)
                return s
       )

-- | Bind a socket to an address.
--
--   - Calling `bind` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - This operation automatically retries on @EINPROGRESS@ and @EALREADY@ and these exceptions won't be thrown.
--   - The following `SocketException`s are relevant and might be thrown (see @man bind@ for more exceptions regarding Unix sockets):
--
--     [@EADDRINUSE@]     The address is in use.
--     [@EADDRNOTAVAIL@]  The address is not available.
--     [@EBADF@]          Not a valid file descriptor.
--     [@EINVAL@]         Socket is already bound and cannot be re-bound or the socket has been shut down.
--     [@ENOBUFS@]        Insufficient resources.
--     [@EOPNOTSUPP@]     The socket type does not support binding.
--     [@EACCES@]         The address is protected and the process is lacking permission.
--     [@EISCONN@]        The socket is already connected.
--     [@ELOOP@]          More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the pathname in address.
--     [@ENAMETOOLONG@]   The length of a pathname exceeds {PATH_MAX}, or pathname resolution of a symbolic link produced an intermediate result  with  a  length  that  exceeds {PATH_MAX}.
--
--   - The following `SocketException`s are theoretically possible, but should not occur if the library is correct:
--
--     [@EAFNOSUPPORT@]   The address family is invalid.
--     [@ENOTSOCK@]       The file descriptor is not a socket.
--     [@EINVAL@]         Address length does not match address family.
bind :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> SockAddr f -> IO ()
bind (Socket mfd) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    fix $ \retry-> do
      i <- withMVar mfd $ \fd-> do
        c_bind fd (castPtr addrPtr :: Ptr ()) (sizeOf addr)
      if i < 0 then do
        e <- getErrno
        if e == eINPROGRESS || e == eALREADY
          then threadWaitWriteMVar mfd >> retry
          else throwIO (SocketException e)
      else do
        return ()

-- | Accept connections on a connection-mode socket.
--
--   - Calling `listen` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - The second parameter is called /backlog/ and sets a limit on how many
--     unaccepted connections the socket implementation shall queue. A value
--     of @0@ leaves the decision to the implementation.
--   - This operation throws `SocketException`s:
--
--     [@EBADF@]          Not a valid file descriptor (only after socket has been closed).
--     [@EDESTADDRREQ@]   The socket is not bound and the protocol does not support listening on an unbound socket.
--     [@EINVAL@]         The socket is already connected or has been shut down.
--     [@ENOTSOCK@]       The file descriptor is not a socket (should be impossible).
--     [@EOPNOTSUPP@]     The protocol does not support listening.
--     [@EACCES@]         The process is lacking privileges.
--     [@ENOBUFS@]        Insufficient resources.
listen :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> Int -> IO ()
listen (Socket ms) backlog = do
  i <- withMVar ms $ \s-> do
    c_listen s backlog
  if i < 0 then do
    getErrno >>= throwIO . SocketException
  else do
    return ()

-- | Accept a new connection.
--
--   - Calling `accept` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - This operation configures the new socket non-blocking (TODO: use `accept4` if available).
--   - This operation sets up a finalizer for the new socket that automatically closes the socket
--     when the garbage collection decides to collect it. This is just a
--     fail-safe. You might still run out of file descriptors as there's
--     no guarantee about when the finalizer is run. You're advised to
--     manually `close` the socket when it's no longer needed.
--   - This operation catches @EAGAIN@, @EWOULDBLOCK@ and @EINTR@ internally and retries automatically.
--   - This operation throws `SocketException`s:
--
--     [@EBADF@]        Not a valid file descriptor (only after the socket has been closed).
--     [@ECONNABORTED@] A connection has been aborted.
--     [@EINVAL@]       The socket is not accepting/listening.
--     [@EMFILE@]       The process is out file descriptors.
--     [@ENFILE@]       The system is out file descriptors.
--     [@ENOBUFS@]      No buffer space available.
--     [@ENOMEM@]       Out of memory.
--     [@ENOSOCK@]      Not a valid socket descriptor (should be impossible).
--     [@EOPNOTSUPP@]   The socket type does not support accepting connections.
--     [@EPROTO@]       Generic protocol error.
accept :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> IO (Socket f t p, SockAddr f)
accept s@(Socket mfd) = acceptWait
  where
    acceptWait :: forall f t p. (AddressFamily f, Type t, Protocol  p) => IO (Socket f t p, SockAddr f)
    acceptWait = do
      -- This is the blocking operation waiting for an event.
      threadWaitReadMVar mfd
      -- We mask asynchronous exceptions during this critical section.
      msa <- withMVarMasked mfd $ \fd-> do
        -- Allocate local (!) memory for the address.
        alloca $ \addrPtr-> do
          alloca $ \addrPtrLen-> do
          -- Oh Haskell, my beauty
            fix $ \retry-> do
              poke addrPtrLen (sizeOf (undefined :: SockAddr f))
              ft <- c_accept fd addrPtr addrPtrLen
              if ft < 0 then do
                e <- getErrno
                if e == eWOULDBLOCK || e == eAGAIN
                  then return Nothing
                  else if e == eINTR
                    -- On EINTR it is good practice to just retry.
                    then retry
                    else throwIO (SocketException e)
              -- This is the critical section: We got a valid descriptor we have not yet returned.
              else do 
                -- setNonBlockingFD calls c_fcntl_write which is an unsafe FFI call.
                let Fd t = ft in setNonBlockingFD t True -- FIXME: throws exception
                -- This peek operation might be a little expensive, but I don't see an alternative.
                addr <- peek addrPtr :: IO (SockAddr f)
                -- newMVar is guaranteed to be not interruptible.
                mft <- newMVar ft
                -- Register a finalizer on the new socket.
                _ <- mkWeakMVar mft (close (Socket mft `asTypeOf` s))
                return (Just (Socket mft, addr))
      -- If msa is Nothing we got EAGAIN or EWOULDBLOCK and retry after the next event.
      case msa of
        Just sa -> return sa
        Nothing -> acceptWait

-- | Connects to an remote address.
--
--   - Calling `connect` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - This operation blocks until either the connection has been established
--     or throws an exception in case the connection attempt failed.
--     @EINTR@, @EINPROGRESS@ and @EALREADY@ are handled internally and won't be thrown.
--   - __Do not__ call `connect` twice from different threads as this imposes a race condition.
--     Calling `connect` a second time after the first call return or threw is safe though.
--     The semantics of doing so is protocol specific.
--   - The following `SocketException`s are relevant and might be thrown:
--
--     [@EADDRNOTAVAIL@] The address is not available.
--     [@EBADF@]         The file descriptor is invalid.
--     [@ECONNREFUSED@]  The target was not listening or refused the connection.
--     [@EISCONN@]       The socket is already connected.
--     [@ENETUNREACH@]   The network is unreachable.
--     [@ETIMEDOUT@]     The connect timed out before a connection was established.
--
--   - The following `SocketException`s are theoretically possible, but should not occur if the library is correct:
--
--     [@EAFNOTSUPPORT@] Address family does not match the socket.
--     [@ENOTSOCK@]      The descriptor is not a socket.
--     [@EPROTOTYPE@]    The address type does not match the socket.
connect :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> SockAddr f -> IO ()
connect (Socket mfd) addr = do
  mwait <- withMVar mfd $ \fd-> do
    when (fd < 0) $ do
      throwIO (SocketException eBADF)
    alloca $ \addrPtr-> do
      poke addrPtr addr
      fix $ \retry-> do
        i <- c_connect fd (castPtr addrPtr :: Ptr ()) (sizeOf addr)
        if i < 0 then do
          e <- getErrno
          if (e == eINTR) then do
            retry
          else if e == eINPROGRESS then do
            -- During the first iteration we get EINPROGRESS and return here.
            -- Register waiting on the descriptor.
            wait <- threadWaitWrite' fd
            return (Just wait)
          else do
            throwIO (SocketException e)
        else do
          -- This should not be the case on non-blocking socket, but better safe than sorry.
          return Nothing
  case mwait of
    Nothing -> return ()
    Just wait -> do
      wait
      -- FIXME: Is this a race condition?
      -- `c_getsockopt` might not reflect the result of the `connect` call, because
      -- we don't hold the lock right here.
      withMVar mfd $ \fd-> do
        alloca $ \errPtr-> do
          alloca $ \errPtrLen-> do
            poke errPtrLen (sizeOf (undefined :: CInt))
            i <- c_getsockopt fd (#const SOL_SOCKET) (#const SO_ERROR)
                                 (errPtr :: Ptr CInt) (errPtrLen :: Ptr Int)
            e <- peek errPtr
            if i < 0 then do
              throwIO (SocketException (Errno e))
            else do
              return ()

-- | Send a message on a connected socket.
--
--   - Calling `send` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - The operation returns the number of bytes sent.
--   - @EAGAIN@, @EWOULDBLOCK@ and @EINTR@ and handled internally and won't be thrown.
--   - The flag @MSG_NOSIGNAL@ is set to supress signals which are pointless.
--   - The following `SocketException`s are relevant and might be thrown:
--
--     [@EBADF@]         The file descriptor is invalid.
--     [@ECONNRESET@]    The peer forcibly closed the connection.
--     [@EDESTADDREQ@]   Remote address has not been set, but is required.
--     [@EMSGSIZE@]      The message is too large to be sent all at once, but the protocol requires this.
--     [@ENOTCONN@]      The socket is not connected.
--     [@EPIPE@]         The socket is shut down for writing or the socket is not connected anymore.
--     [@EACCESS@]       The process is lacking permissions.
--     [@EIO@]           An I/O error occured while writing to the filesystem.
--     [@ENETDOWN@]      The local network interface is down.
--     [@ENETUNREACH@]   No route to network.
--     [@ENOBUFS@]       Insufficient resources to fulfill the request.
--
--   - The following `SocketException`s are theoretically possible, but should not occur if the library is correct:
--
--     [@EOPNOTSUPP@]    The specified flags are not supported.
--     [@ENOTSOCK@]      The descriptor does not refer to a socket.
send :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> BS.ByteString -> IO Int
send (Socket mfd) bs =
  fix $ \wait-> do
    threadWaitWriteMVar mfd
    bytesSend <- withMVar mfd $ \fd-> do
      when (fd < 0) $ do
        throwIO (SocketException eBADF)
      BS.unsafeUseAsCStringLen bs $ \(ptr,len)->
        fix $ \retry-> do
          i <- c_send fd ptr len (#const MSG_NOSIGNAL)
          if (i < 0) then do
            e <- getErrno
            if e == eWOULDBLOCK || e == eAGAIN 
              then return i
            else if e == eINTR
              then retry
              else throwIO (SocketException e)
          -- Send succeeded. Return the bytes send.
          else return i
    -- We cannot loop from within the block above, because this would keep the MVar locked.
    if bytesSend < 0
      then wait
      else return bytesSend

-- | Send a message on a socket with a specific destination address.
--
--   - Calling `sendTo` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - The operation returns the number of bytes sent.
--   - @EAGAIN@, @EWOULDBLOCK@ and @EINTR@ and handled internally and won't be thrown.
--   - The flag @MSG_NOSIGNAL@ is set to supress signals which are pointless.
--   - The following `SocketException`s are relevant and might be thrown:
--
--     [@EBADF@]         The file descriptor is invalid.
--     [@ECONNRESET@]    The peer forcibly closed the connection.
--     [@EDESTADDREQ@]   Remote address has not been set, but is required.
--     [@EMSGSIZE@]      The message is too large to be sent all at once, but the protocol requires this.
--     [@ENOTCONN@]      The socket is not connected.
--     [@EPIPE@]         The socket is shut down for writing or the socket is not connected anymore.
--     [@EACCESS@]       The process is lacking permissions.
--     [@EDESTADDRREQ@]  The destination address is required.
--     [@EHOSTUNREACH@]  The destination host cannot be reached.
--     [@EIO@]           An I/O error occured.
--     [@EISCONN@]       The socket is already connected.
--     [@ENETDOWN@]      The local network is down.
--     [@ENETUNREACH@]   No route to the network.
--     [@ENUBUFS@]       Insufficient resources to fulfill the request.
--     [@ENOMEM@]        Insufficient memory to fulfill the request.
--     [@ELOOP@]         @AF_UNIX@ only.
--     [@ENAMETOOLONG@]  @AF_UNIX@ only.
--
--   - The following `SocketException`s are theoretically possible, but should not occur if the library is correct:
--
--     [@EAFNOTSUPP@]    The address family does not match.
--     [@EOPNOTSUPP@]    The specified flags are not supported.
--     [@ENOTSOCK@]      The descriptor does not refer to a socket.
--     [@EINVAL@]        The address len does not match.
sendTo :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> BS.ByteString -> SockAddr f -> IO Int
sendTo (Socket mfd) bs addr =
  alloca $ \addrPtr-> do
    poke addrPtr addr
    fix $ \wait-> do
      threadWaitWriteMVar mfd
      bytesSend <- withMVar mfd $ \fd-> do
        when (fd < 0) $ do
          throwIO (SocketException eBADF)
        BS.unsafeUseAsCStringLen bs $ \(msgPtr,msgLen)-> do
          fix $ \retry-> do
            i <- c_sendto fd msgPtr msgLen (#const MSG_NOSIGNAL) (castPtr addrPtr) (sizeOf addr)
            if (i < 0) then do
              e <- getErrno
              if e == eWOULDBLOCK || e == eAGAIN
                then return i
              else if e == eINTR
                then retry
                else throwIO (SocketException e)
            -- Send succeeded. Return the bytes send.
            else return i
      -- We cannot loop from within the block above, because this would keep the MVar locked.
      if bytesSend < 0
        then wait
        else return bytesSend

-- | Receive a message on a connected socket.
--
--   - Calling `recv` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - The operation takes a buffer size in bytes a first parameter which
--     limits the maximum length of the returned `Data.ByteString.ByteString`.
--   - @EAGAIN@, @EWOULDBLOCK@ and @EINTR@ and handled internally and won't be thrown.
--   - The following `SocketException`s are relevant and might be thrown:
--
--     [@EBADF@]         The file descriptor is invalid.
--     [@ECONNRESET@]    The peer forcibly closed the connection.
--     [@ENOTCONN@]      The socket is not connected.
--     [@ETIMEDOUT@]     The connection timed out.
--     [@EIO@]           An I/O error occured while writing to the filesystem.
--     [@ENOBUFS@]       Insufficient resources to fulfill the request.
--     [@ENONMEM@]       Insufficient memory to fulfill the request.
--
--   - The following `SocketException`s are theoretically possible, but should not occur if the library is correct:
--
--     [@EOPNOTSUPP@]    The specified flags are not supported.
--     [@ENOTSOCK@]      The descriptor does not refer to a socket.
recv :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> Int -> IO BS.ByteString
recv (Socket mfd) bufSize =
  fix $ \wait-> do
    threadWaitReadMVar mfd
    mbs <- withMVarMasked mfd $ \fd-> do
      when (fd < 0) $ do
        throwIO (SocketException eBADF)
      ptr <- mallocBytes bufSize
      fix $ \retry-> do
        i <- c_recv fd ptr bufSize 0
        if (i < 0) then do
          e <- getErrno
          if e == eWOULDBLOCK || e == eAGAIN 
            then do
              -- At this exit we need to free the pointer manually.
              free ptr
              return Nothing
          else if e == eINTR
            then retry
            else do
              -- At this exit we need to free the pointer manually as well.
              free ptr
              throwIO (SocketException e)
        else do
          -- Send succeeded, generate a ByteString.
          -- From now on the ByteString takes care of freeing the pointer somewhen in the future.
          -- The resulting ByteString might be shorter than the malloced buffer.
          -- This is a fair tradeoff as otherwise we had to create a fresh copy.
          bs <- BS.unsafePackMallocCStringLen (ptr, i)
          return (Just bs)
    -- We cannot loop from within the block above, because this would keep the MVar locked.
    case mbs of
      Nothing -> wait
      Just bs -> return bs

-- | Receive a message on a socket and additionally yield the peer address.
--
--   - Calling `recvFrom` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - The operation takes a buffer size in bytes a first parameter which
--     limits the maximum length of the returned `Data.ByteString.ByteString`.
--   - @EAGAIN@, @EWOULDBLOCK@ and @EINTR@ and handled internally and won't be thrown.
--   - The following `SocketException`s are relevant and might be thrown:
--
--     [@EBADF@]         The file descriptor is invalid.
--     [@ECONNRESET@]    The peer forcibly closed the connection.
--     [@ENOTCONN@]      The socket is not connected.
--     [@ETIMEDOUT@]     The connection timed out.
--     [@EIO@]           An I/O error occured while writing to the filesystem.
--     [@ENOBUFS@]       Insufficient resources to fulfill the request.
--     [@ENONMEM@]       Insufficient memory to fulfill the request.
--
--   - The following `SocketException`s are theoretically possible, but should not occur if the library is correct:
--
--     [@EOPNOTSUPP@]    The specified flags are not supported.
--     [@ENOTSOCK@]      The descriptor does not refer to a socket.
recvFrom :: forall f t p. (AddressFamily f, Type t, Protocol  p) => Socket f t p -> Int -> IO (BS.ByteString, SockAddr f)
recvFrom (Socket mfd) bufSize =
  alloca $ \addrPtr-> do
    alloca $ \addrPtrLen-> do
      poke addrPtrLen (sizeOf (undefined :: SockAddr f))
      fix $ \wait-> do
        threadWaitReadMVar mfd
        mbsa <- withMVarMasked mfd $ \fd-> do
          when (fd < 0) $ do
            throwIO (SocketException eBADF)
          ptr <- mallocBytes bufSize
          fix $ \retry-> do
            i <- c_recvfrom fd ptr bufSize 0 (castPtr addrPtr) addrPtrLen
            if (i < 0) then do
              e <- getErrno
              if e == eWOULDBLOCK || e == eAGAIN 
                then do
                  -- At this exit we need to free the pointer manually.
                  free ptr
                  return Nothing
              else if e == eINTR
                then retry
                else do
                  -- At this exit we need to free the pointer manually as well.
                  free ptr
                  throwIO (SocketException e)
            else do
              -- Send succeeded, generate a ByteString.
              -- From now on the ByteString takes care of freeing the pointer somewhen in the future.
              -- The resulting ByteString might be shorter than the malloced buffer.
              -- This is a fair tradeoff as otherwise we had to create a fresh copy.
              bs   <- BS.unsafePackMallocCStringLen (ptr, i)
              addr <- peek addrPtr
              return (Just (bs,addr))
        -- We cannot loop from within the block above, because this would keep the MVar locked.
        case mbsa of
          Nothing -> wait
          Just bsa -> return bsa

-- | Closes a socket.
--
--   - This operation is idempotent and thus can be performed more than once without throwing an exception.
--     If it throws an exception it is presumably a not recoverable situation and the process should exit.
--   - This operation does not block.
--   - This operation wakes up all threads that are currently blocking on this
--     socket. All other threads are guaranteed not to block on operations on this socket in the future.
--     Threads that perform operations other than `close` on this socket will fail with @EBADF@
--     after the socket has been closed (`close` replaces the 
--     `System.Posix.Types.Fd` in the `Control.Concurrent.MVar.MVar` with @-1@
--     to reliably avoid use-after-free situations).
--   - The following `SocketException`s are relevant and might be thrown:
--
--     [@EIO@]           An I/O error occured while writing to the filesystem.
--
--   - The following `SocketException`s are theoretically possible, but should not occur if the library is correct:
--
--     [@EBADF@]         The file descriptor is invalid.
close :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> IO ()
close (Socket mfd) = do
  modifyMVarMasked_ mfd $ \fd-> do
    if fd < 0 then do
      return fd
    else do
      -- closeFdWith does not throw even on invalid file descriptors.
      -- It just assures no thread is blocking on the fd anymore and then executes the IO action.
      closeFdWith
        -- The c_close operation may (according to Posix documentation) fails with EINTR or EBADF or EIO.
        -- EBADF: Should be ruled out by the library's design.
        -- EINTR: It is best practice to just retry the operation what we do here.
        -- EIO: Only occurs when filesystem is involved (?).
        -- Conclusion: Our close should never fail. If it does, something is horribly wrong.
        ( const $ fix $ \retry-> do
            i <- c_close fd
            if i < 0 then do
              e <- getErrno
              if e == eINTR 
                then retry
                else throwIO (SocketException e)
            else return ()
        ) fd
      -- When we arrive here, no exception has been thrown and the descriptor has been closed.
      -- We put an invalid file descriptor into the MVar.
      return (-1)

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
-- SockOpt
-------------------------------------------------------------------------------

class GetSockOpt o where
  getSockOpt :: Socket f t p -> IO o

class SetSockOpt o where
  setSockOpt :: Socket f t p -> o -> IO ()

data SO_ACCEPTCONN
   = SO_ACCEPTCONN Bool

instance GetSockOpt SO_ACCEPTCONN where
  getSockOpt (Socket mfd) = do
    withMVar mfd $ \fd->
      alloca $ \vPtr-> do
        alloca $ \lPtr-> do
          i <- c_getsockopt fd (#const SOL_SOCKET) (#const SO_ACCEPTCONN) (vPtr :: Ptr Int) (lPtr :: Ptr Int)
          if i < 0 then do
            throwIO . SocketException =<< getErrno
          else do
            v <- peek vPtr
            return $ SO_ACCEPTCONN (v == 1)

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

-------------------------------------------------------------------------------
-- Exceptions
-------------------------------------------------------------------------------

newtype SocketException = SocketException Errno
  deriving Typeable

instance Exception SocketException

instance Show SocketException where
  show (SocketException e) = "SocketException " ++ strerr e
    where 
      strerr errno
        | errno == eOK             = "EOK"
        | errno == e2BIG           = "E2BIG"
        | errno == eACCES          = "EACCES"
        | errno == eADDRINUSE      = "EADDRINUSE"
        | errno == eADDRNOTAVAIL   = "EADDRNOTAVAIL"
        | errno == eADV            = "EADV"
        | errno == eAFNOSUPPORT    = "EAFNOSUPPORT"
        | errno == eAGAIN          = "EAGAIN"
        | errno == eALREADY        = "EALREADY"
        | errno == eBADF           = "EBADF"
        | errno == eBADMSG         = "EBADMSG"
        | errno == eBADRPC         = "EBADRPC"
        | errno == eBUSY           = "EBUSY"
        | errno == eCHILD          = "ECHILD"
        | errno == eCOMM           = "ECOMM"
        | errno == eCONNABORTED    = "ECONNABORTED"
        | errno == eCONNREFUSED    = "ECONNREFUSED"
        | errno == eCONNRESET      = "ECONNRESET"
        | errno == eDEADLK         = "EDEADLK"
        | errno == eDESTADDRREQ    = "EDESTADDRREQ"
        | errno == eDIRTY          = "EDIRTY"
        | errno == eDOM            = "EDOM"
        | errno == eDQUOT          = "EDQUOT"
        | errno == eEXIST          = "EEXIST"
        | errno == eFAULT          = "EFAULT"
        | errno == eFBIG           = "EFBIG"
        | errno == eFTYPE          = "EFTYPE"
        | errno == eHOSTDOWN       = "EHOSTDOWN"
        | errno == eHOSTUNREACH    = "EHOSTUNREACH"
        | errno == eIDRM           = "EIDRM"
        | errno == eILSEQ          = "EILSEQ"
        | errno == eINPROGRESS     = "EINPROGRESS"
        | errno == eINTR           = "EINTR"
        | errno == eINVAL          = "EINVAL"
        | errno == eIO             = "EIO"
        | errno == eISCONN         = "EISCONN"
        | errno == eISDIR          = "EISDIR"
        | errno == eLOOP           = "ELOOP"
        | errno == eMFILE          = "EMFILE"
        | errno == eMLINK          = "EMLINK"
        | errno == eMSGSIZE        = "EMSGSIZE"
        | errno == eMULTIHOP       = "EMULTIHOP"
        | errno == eNAMETOOLONG    = "ENAMETOOLONG"
        | errno == eNETDOWN        = "ENETDOWN"
        | errno == eNETRESET       = "ENETRESET"
        | errno == eNETUNREACH     = "ENETUNREACH"
        | errno == eNFILE          = "ENFILE"
        | errno == eNOBUFS         = "ENOBUFS"
        | errno == eNODATA         = "ENODATA"
        | errno == eNODEV          = "ENODEV"
        | errno == eNOENT          = "ENOENT"
        | errno == eNOEXEC         = "ENOEXEC"
        | errno == eNOLCK          = "ENOLCK"
        | errno == eNOLINK         = "ENOLINK"
        | errno == eNOMEM          = "ENOMEM"
        | errno == eNOMSG          = "ENOMSG"
        | errno == eNONET          = "ENONET"
        | errno == eNOPROTOOPT     = "ENOPROTOOPT"
        | errno == eNOSPC          = "ENOSPC"
        | errno == eNOSR           = "ENOSR"
        | errno == eNOSTR          = "ENOSTR"
        | errno == eNOSYS          = "ENOSYS"
        | errno == eNOTBLK         = "ENOTBLK"
        | errno == eNOTCONN        = "ENOTCONN"
        | errno == eNOTDIR         = "ENOTDIR"
        | errno == eNOTEMPTY       = "ENOTEMPTY"
        | errno == eNOTSOCK        = "ENOTSOCK"
        | errno == eNOTTY          = "ENOTTY"
        | errno == eNXIO           = "ENXIO"
        | errno == eOPNOTSUPP      = "EOPNOTSUPP"
        | errno == ePERM           = "EPERM"
        | errno == ePFNOSUPPORT    = "EPFNOSUPPORT"
        | errno == ePIPE           = "EPIPE"
        | errno == ePROCLIM        = "EPROCLIM"
        | errno == ePROCUNAVAIL    = "EPROCUNAVAIL"
        | errno == ePROGMISMATCH   = "EPROGMISMATCH"
        | errno == ePROGUNAVAIL    = "EPROGUNAVAIL"
        | errno == ePROTO          = "EPROTO"
        | errno == ePROTONOSUPPORT = "EPROTONOSUPPORT"
        | errno == ePROTOTYPE      = "EPROTOTYPE"
        | errno == eRANGE          = "ERANGE"
        | errno == eREMCHG         = "EREMCHG"
        | errno == eREMOTE         = "EREMOTE"
        | errno == eROFS           = "EROFS"
        | errno == eRPCMISMATCH    = "ERPCMISMATCH"
        | errno == eRREMOTE        = "ERREMOTE"
        | errno == eSHUTDOWN       = "ESHUTDOWN"
        | errno == eSOCKTNOSUPPORT = "ESOCKTNOSUPPORT"
        | errno == eSPIPE          = "ESPIPE"
        | errno == eSRCH           = "ESRCH"
        | errno == eSRMNT          = "ESRMNT"
        | errno == eSTALE          = "ESTALE"
        | errno == eTIME           = "ETIME"
        | errno == eTIMEDOUT       = "ETIMEDOUT"
        | errno == eTOOMANYREFS    = "ETOOMANYREFS"
        | errno == eTXTBSY         = "ETXTBSY"
        | errno == eUSERS          = "EUSERS"
        | errno == eWOULDBLOCK     = "EWOULDBLOCK"
        | errno == eXDEV           = "EXDEV"
        | otherwise                = let Errno i = errno
                                     in  show i


-------------------------------------------------------------------------------
-- FFI
-------------------------------------------------------------------------------

foreign import ccall unsafe "sys/socket.h socket"
  c_socket  :: CInt -> CInt -> CInt -> IO Fd

foreign import ccall unsafe "unistd.h close"
  c_close   :: Fd -> IO CInt

foreign import ccall unsafe "sys/socket.h bind"
  c_bind    :: Fd -> Ptr a -> Int -> IO CInt

foreign import ccall unsafe "sys/socket.h connect"
  c_connect :: Fd -> Ptr a -> Int -> IO CInt

foreign import ccall unsafe "sys/socket.h accept"
  c_accept  :: Fd -> Ptr a -> Ptr Int -> IO Fd

foreign import ccall unsafe "sys/socket.h listen"
  c_listen  :: Fd -> Int -> IO CInt

foreign import ccall unsafe "sys/socket.h send"
  c_send    :: Fd -> Ptr CChar -> Int -> Int -> IO Int

foreign import ccall unsafe "sys/socket.h sendto"
  c_sendto  :: Fd -> Ptr CChar -> Int -> Int -> Ptr CChar -> Int -> IO Int

foreign import ccall unsafe "sys/socket.h recv"
  c_recv    :: Fd -> Ptr CChar -> Int -> Int -> IO Int

foreign import ccall unsafe "sys/socket.h recvfrom"
  c_recvfrom :: Fd -> Ptr CChar -> Int -> Int -> Ptr CChar -> Ptr Int -> IO Int

foreign import ccall unsafe "sys/socket.h getsockopt"
  c_getsockopt  :: Fd -> CInt -> CInt -> Ptr a -> Ptr Int -> IO CInt