{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
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
-- >   s <- socket :: IO (Socket SockAddrIn STREAM TCP)
-- >   bind s (SockAddrIn 8080 (pack [127,0,0,1]))
-- >   listen s 5
-- >   forever $ do
-- >     (peer,addr) <- accept s
-- >     forkIO $ do
-- >       send peer "Hello world!"
-- >       close peer
-----------------------------------------------------------------------------
module System.Socket (
  -- * Name Resolution
    AddrInfo (..)
  -- ** getAddrInfo
  , getAddrInfo
  -- ** getNameInfo
  , getNameInfo
  -- * Operations
  -- ** socket
  , socket
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
  -- ** sendAll
  , sendAll
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
  -- ** Addresses
  , Address (..)
  -- *** SockAddrIn
  , SockAddrIn (..)
  -- *** SockAddrIn6
  , SockAddrIn6 (..)
  -- *** SockAddrUn
  , SockAddrUn (..)
  -- ** Types
  , Type (..)
  -- *** DGRAM
  , DGRAM
  -- *** STREAM
  , STREAM
  -- *** SEQPACKET
  , SEQPACKET
  -- ** Protocols
  , Protocol  (..)
  -- *** UDP
  , UDP
  -- *** TCP
  , TCP
  -- *** SCTP
  , SCTP
  -- * Exceptions
  -- ** SocketException
  , SocketException (..)
  -- ** AddrInfoException
  , AddrInfoException (..)
  -- * Options
  , GetSockOpt (..)
  , SetSockOpt (..)
  -- ** SO_ACCEPTCONN
  , SO_ACCEPTCONN (..)
  -- * Flags
  -- ** MsgFlags
  , MsgFlags (..)
  , msgEOR
  , msgNOSIGNAL
  , msgOOB
  , msgWAITALL
  -- ** AddrInfoFlags
  , AddrInfoFlags (..)
  , aiADDRCONFIG
  , aiALL
  , aiCANONNAME
  , aiNUMERICHOST
  , aiNUMERICSERV
  , aiPASSIVE
  , aiV4MAPPED
  -- ** NameInfoFlags
  , NameInfoFlags (..)
  , niNAMEREQD
  , niDGRAM
  , niNOFQDN
  , niNUMERICHOST
  , niNUMERICSERV
  ) where

import Control.Exception
import Control.Monad
import Control.Concurrent.MVar

import Data.Function
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import GHC.Conc (closeFdWith)

import Foreign.C.Error
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.Socket.Unsafe

import System.Socket.Internal.Socket
import System.Socket.Internal.Event
import System.Socket.Internal.FFI
import System.Socket.Internal.Exception
import System.Socket.Internal.MsgFlags
import System.Socket.Internal.AddrInfo

import System.Socket.Address
import System.Socket.Address.SockAddrUn
import System.Socket.Address.SockAddrIn
import System.Socket.Address.SockAddrIn6

import System.Socket.Type
import System.Socket.Type.STREAM
import System.Socket.Type.DGRAM
import System.Socket.Type.SEQPACKET

import System.Socket.Protocol
import System.Socket.Protocol.UDP
import System.Socket.Protocol.TCP
import System.Socket.Protocol.SCTP

#include "sys/socket.h"

-- | Creates a new socket.
--
--   Whereas the underlying POSIX socket function takes 3 parameters, this library
--   encodes this information in the type variables. This rules out several
--   kinds of errors and escpecially simplifies the handling of addresses (by using
--   associated type families). Examples:
--
--   > -- create a IPv4-UDP-datagram socket
--   > sock <- socket :: IO (Socket SockAddrIn DGRAM UDP)
--   > -- create a IPv6-TCP-streaming socket
--   > sock6 <- socket :: IO (Socket SockAddrIn6 STREAM TCP)
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
socket :: (Address a, Type t, Protocol  p) => IO (Socket a t p)
socket = socket'
 where
   socket' :: forall a t p. (Address a, Type t, Protocol  p) => IO (Socket a t p)
   socket'  = do
     bracketOnError
       -- Try to acquire the socket resource. This part has exceptions masked.
       ( c_socket (addressFamilyNumber (undefined :: a)) (typeNumber (undefined :: t)) (protocolNumber (undefined :: p)) )
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
                i <- c_setnonblocking fd
                if i < 0 then do
                  getErrno >>= throwIO . SocketException
                else do
                  mfd <- newMVar fd
                  let s = Socket mfd
                  _ <- mkWeakMVar mfd (close s)
                  return s
       )

-- | Bind a socket to an address.
--
--   - Calling `bind` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - It is assumed that `c_bind` never blocks and therefore @EINPROGRESS@, @EALREADY@ and @EINTR@ don't occur.
--     This assumption is supported by the fact that the Linux manpage doesn't mention any of these errors,
--     the Posix manpage doesn't mention the last one and even MacOS' implementation will never
--     fail with any of these when the socket is configured non-blocking as
--     [argued here](http://stackoverflow.com/a/14485305).
--   - The following `SocketException`s are relevant and might be thrown (see @man bind@ for more exceptions regarding SockAddrUn sockets):
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
bind :: (Address a, Type t, Protocol  p) => Socket a t p -> a -> IO ()
bind (Socket mfd) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    withMVar mfd $ \fd-> do
      i <- c_bind fd addrPtr (fromIntegral $ sizeOf addr)
      if i < 0
        then getErrno >>= throwIO . SocketException
        else return ()

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
listen :: (Address a, Type t, Protocol  p) => Socket a t p -> Int -> IO ()
listen (Socket ms) backlog = do
  i <- withMVar ms $ \s-> do
    c_listen s (fromIntegral backlog)
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
accept :: (Address a, Type t, Protocol  p) => Socket a t p -> IO (Socket a t p, a)
accept s@(Socket mfd) = accept'
  where
    accept' :: forall a t p. (Address a, Type t, Protocol  p) => IO (Socket a t p, a)
    accept' = do
      -- Allocate local (!) memory for the address.
      alloca $ \addrPtr-> do
        alloca $ \addrPtrLen-> do
          poke addrPtrLen (fromIntegral $ sizeOf (undefined :: a))
          fix $ \again-> do
            -- We mask asynchronous exceptions during this critical section.
            ews <- withMVarMasked mfd $ \fd-> do
              fix $ \retry-> do
                ft <- c_accept fd addrPtr addrPtrLen
                if ft < 0 then do
                  e <- getErrno
                  if e == eWOULDBLOCK || e == eAGAIN
                    then threadWaitRead' fd >>= return . Left
                    else if e == eINTR
                      -- On EINTR it is good practice to just retry.
                      then retry
                      else throwIO (SocketException e)
                -- This is the critical section: We got a valid descriptor we have not yet returned.
                else do 
                  i <- c_setnonblocking ft
                  if i < 0 then do
                    getErrno >>= throwIO . SocketException
                  else do
                    -- This peek operation might be a little expensive, but I don't see an alternative.
                    addr <- peek addrPtr :: IO (a)
                    -- newMVar is guaranteed to be not interruptible.
                    mft <- newMVar ft
                    -- Register a finalizer on the new socket.
                    _ <- mkWeakMVar mft (close (Socket mft `asTypeOf` s))
                    return (Right (Socket mft, addr))
            -- If ews is Left we got EAGAIN or EWOULDBLOCK and retry after the next event.
            case ews of
              Left  wait -> wait >> again
              Right sock -> return sock

-- | Connects to an remote address.
--
--   - Calling `connect` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - This function returns as soon as a connection has either been established
--     or refused. A failed connection attempt does not throw an exception
--     if @EINTR@ or @EINPROGRESS@ were caught internally. The operation
--     just unblocks and returns in this case. The approach is to
--     just try to read or write the socket and eventually fail there instead.
--     Also see [these considerations](http://cr.yp.to/docs/connect.html) for an explanation.
--     @EINTR@ and @EINPROGRESS@ are handled internally and won't be thrown.
--   - The following `SocketException`s are relevant and might be thrown if the
--     OS was able to decide the connection request synchronously:
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
connect :: (Address a, Type t, Protocol  p) => Socket a t p -> a -> IO ()
connect (Socket mfd) addr = do
  mwait <- withMVar mfd $ \fd-> do
    when (fd < 0) $ do
      throwIO (SocketException eBADF)
    alloca $ \addrPtr-> do
      poke addrPtr addr
      i <- c_connect fd addrPtr (fromIntegral $ sizeOf addr)
      if i < 0 then do
        e <- getErrno
        if e == eINPROGRESS || e == eINTR then do
          -- The manpage says that in this case the connection
          -- shall be established asynchronously and one is
          -- supposed to wait.
          wait <- threadWaitWrite' fd
          return (Just wait)
        else do
          throwIO (SocketException e)
      else do
        -- This should not be the case on non-blocking socket, but better safe than sorry.
        return Nothing
  case mwait of
    Just wait -> wait
    Nothing   -> return ()

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
send :: (Address a, Type t, Protocol  p) => Socket a t p -> BS.ByteString -> MsgFlags -> IO Int
send s bs flags = do
  bytesSent <- BS.unsafeUseAsCStringLen bs $ \(bufPtr,bufSize)->
    unsafeSend s bufPtr (fromIntegral bufSize) flags
  return (fromIntegral bytesSent)

-- | Like `send`, but continues until all data has been sent.
--
--   > sendAll sock data flags = do
--   >   sent <- send sock data flags
--   >   when (sent < length data) $ sendAll sock (drop sent data) flags
sendAll ::(Address a, Type t, Protocol  p) => Socket a t p -> BS.ByteString -> MsgFlags -> IO ()
sendAll s bs flags = do
  sent <- send s bs flags
  when (sent < BS.length bs) $ sendAll s (BS.drop sent bs) flags

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
sendTo :: (Address a, Type t, Protocol  p) => Socket a t p -> BS.ByteString -> MsgFlags -> a -> IO Int
sendTo s bs flags addr = do
  bytesSent <- alloca $ \addrPtr-> do
    poke addrPtr addr
    BS.unsafeUseAsCStringLen bs $ \(bufPtr,bufSize)->
      unsafeSendTo s bufPtr (fromIntegral bufSize) flags addrPtr (fromIntegral $ sizeOf addr)
  return (fromIntegral bytesSent)

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
recv :: (Address a, Type t, Protocol  p) => Socket a t p -> Int -> MsgFlags -> IO BS.ByteString
recv s bufSize flags =
  bracketOnError
    ( mallocBytes bufSize )
    (\bufPtr-> free bufPtr )
    (\bufPtr-> do
        bytesReceived <- unsafeRecv s bufPtr (fromIntegral bufSize) flags
        BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bytesReceived)
    )

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
recvFrom :: forall a t p. (Address a, Type t, Protocol  p) => Socket a t p -> Int -> MsgFlags -> IO (BS.ByteString, a)
recvFrom s bufSize flags =
  alloca $ \addrPtr-> do
    alloca $ \addrSizePtr-> do
      poke addrSizePtr (fromIntegral $ sizeOf (undefined :: a))
      bracketOnError
        ( mallocBytes bufSize )
        (\bufPtr-> free bufPtr )
        (\bufPtr-> do
            bytesReceived <- unsafeRecvFrom s bufPtr (fromIntegral bufSize) flags addrPtr addrSizePtr
            addr <- peek addrPtr
            bs   <- BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bytesReceived)
            return (bs, addr)
        )

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
close :: (Address a, Type t, Protocol  p) => Socket a t p -> IO ()
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

