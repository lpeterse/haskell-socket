{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
--------------------------------------------------------------------------------
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
-- > import Control.Exception ( bracket, catch )
-- > import Control.Monad ( forever )
-- >
-- > import System.Socket
-- > import System.Socket.Family.Inet6
-- > import System.Socket.Type.Stream
-- > import System.Socket.Protocol.TCP
-- >
-- > main :: IO ()
-- > main = bracket
-- >   ( socket :: IO (Socket Inet6 Stream TCP) )
-- >   ( \s-> do
-- >     close s
-- >     putStrLn "Listening socket closed."
-- >   )
-- >   ( \s-> do
-- >     setSocketOption s (ReuseAddress True)
-- >     setSocketOption s (V6Only False)
-- >     bind s (SocketAddressInet6 inet6Any 8080 0 0)
-- >     listen s 5
-- >     putStrLn "Listening socket ready..."
-- >     forever $ acceptAndHandle s `catch` \e-> print (e :: SocketException)
-- >   )
-- >
-- > acceptAndHandle :: Socket Inet6 Stream TCP -> IO ()
-- > acceptAndHandle s = bracket
-- >   ( accept s )
-- >   ( \(p, addr)-> do
-- >     close p
-- >     putStrLn $ "Closed connection to " ++ show addr
-- >   )
-- >   ( \(p, addr)-> do
-- >     putStrLn $ "Accepted connection from " ++ show addr
-- >     sendAll p "Hello world!" msgNoSignal
-- >   )
--------------------------------------------------------------------------------
module System.Socket (
  -- * Socket
    Socket ()
  , SocketAddress ()
  -- ** Family
  , Family (..)
  -- ** Type
  , Type (..)
  -- ** Protocol
  , Protocol  (..)
  -- * Operations
  -- ** socket
  , socket
  -- ** connect
  , connect
  -- ** bind
  , bind
  -- ** listen
  , listen
  -- ** accept
  , accept
  -- ** send, sendTo
  , send, sendTo
  -- ** receive, receiveFrom
  , receive, receiveFrom
  -- ** close
  , close
  -- * Options
  , SocketOption (..)
  , Error (..)
  , ReuseAddress (..)
  -- * Name Resolution
  -- ** getAddressInfo
  , AddressInfo (..)
  , HasAddressInfo (..)
  -- ** getNameInfo
  , NameInfo (..)
  , HasNameInfo (..)
  -- * Flags
  -- ** MessageFlags
  , MessageFlags (..)
  , msgEndOfRecord
  , msgNoSignal
  , msgOutOfBand
  , msgWaitAll
  -- ** AddressInfoFlags
  , AddressInfoFlags ()
  , aiAddressConfig
  , aiAll
  , aiCanonicalName
  , aiNumericHost
  , aiNumericService
  , aiPassive
  , aiV4Mapped
  -- ** NameInfoFlags
  , NameInfoFlags ()
  , niNameRequired
  , niDatagram
  , niNoFullyQualifiedDomainName
  , niNumericHost
  , niNumericService
  -- * Exceptions
  -- ** SocketException
  , module System.Socket.Internal.Exception
  -- ** AddressInfoException
  , AddressInfoException (..)
  , eaiAgain
  , eaiBadFlags
  , eaiFail
  , eaiFamily
  , eaiMemory
  , eaiNoName
  , eaiSocketType
  , eaiService
  , eaiSystem
  ) where

import Control.Exception
import Control.Monad
import Control.Applicative
import Control.Concurrent

import Data.Function
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import GHC.Conc (closeFdWith)

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.Socket.Unsafe

import System.Socket.Internal.Socket
import System.Socket.Internal.Exception
import System.Socket.Internal.Message
import System.Socket.Internal.AddressInfo
import System.Socket.Internal.Platform

#include "hs_socket.h"

-- | Creates a new socket.
--
--   Whereas the underlying POSIX socket operation takes 3 parameters, this library
--   encodes this information in the type variables. This rules out several
--   kinds of errors and escpecially simplifies the handling of addresses (by using
--   associated type families). Examples:
--
--   > -- create a IPv4-UDP-datagram socket
--   > sock <- socket :: IO (Socket Inet Datagram UDP)
--   > -- create a IPv6-TCP-streaming socket
--   > sock6 <- socket :: IO (Socket Inet6 Stream TCP)
--
--     - This operation sets up a finalizer that automatically closes the socket
--       when the garbage collection decides to collect it. This is just a
--       fail-safe. You might still run out of file descriptors as there's
--       no guarantee about when the finalizer is run. You're advised to
--       manually `close` the socket when it's no longer needed.
--       If possible, use `Control.Exception.bracket` to reliably close the
--       socket descriptor on exception or regular termination of your
--       computation:
--
--       > result <- bracket (socket :: IO (Socket Inet6 Stream TCP)) close $ \sock-> do
--       >   somethingWith sock -- your computation here
--       >   return somethingelse
--
--
--     - This operation configures the socket non-blocking to work seamlessly
--       with the runtime system's event notification mechanism.
--     - This operation can safely deal with asynchronous exceptions without
--       leaking file descriptors.
--     - This operation throws `SocketException`s. Consult your @man@ page for
--       details and specific @errno@s.
socket :: (Family f, Type t, Protocol  p) => IO (Socket f t p)
socket = socket'
 where
   socket' :: forall f t p. (Family f, Type t, Protocol  p) => IO (Socket f t p)
   socket'  = alloca $ \errPtr-> do
     bracketOnError
       -- Try to acquire the socket resource. This part has exceptions masked.
       ( c_socket (familyNumber (undefined :: f)) (typeNumber (undefined :: t)) (protocolNumber (undefined :: p)) errPtr )
       -- On failure after the c_socket call we try to close the socket to not leak file descriptors.
       -- If closing fails we cannot really do something about it. We tried at least.
       -- c_close is an unsafe FFI call.
       ( \fd-> when (fd >= 0) $ alloca $ void . c_close fd )
       ( \fd-> do
           when (fd < 0) (SocketException <$> peek errPtr >>= throwIO)
           mfd <- newMVar fd
           let s = Socket mfd
           _ <- mkWeakMVar mfd (close s)
           return s
       )

-- | Connects to a remote address.
--
--   - This operation returns as soon as a connection has been established (as
--     if the socket were blocking). The connection attempt has either failed or
--     succeeded after this operation threw an exception or returned.
--   - The socket is locked throughout the whole operation.
--   - The operation throws `SocketException`s. Calling `connect` on a `close`d
--     socket throws `eBadFileDescriptor` even if the former file descriptor has
--     been reassigned.
connect :: (Family f, Storable (SocketAddress f)) => Socket f t p -> SocketAddress f -> IO ()
connect (Socket mfd) addr =
  withMVar mfd $ \fd-> do
    when (fd < 0) (throwIO eBadFileDescriptor)
    alloca $ \addrPtr-> alloca $ \errPtr-> do
      poke addrPtr addr
      let addrLen = fromIntegral (sizeOf addr)
      -- The actual connection attempt.
      i <- c_connect fd addrPtr addrLen errPtr
      -- On non-blocking sockets we expect to get EINPROGRESS or EWOULDBLOCK.
      when (i /= 0) $ do
        err <- SocketException <$> peek errPtr
        if err == eInProgress || err == eWouldBlock
          then do
            -- The manpage says that in this case the connection
            -- shall be established asynchronously and one is
            -- supposed to wait.
            unsafeSocketWaitConnected fd
            -- At least on Linux a second connect after signaled writeability
            -- will not fail (the next one would).
            i' <- c_connect fd addrPtr addrLen errPtr
            when (i' /= 0) $ do
              err' <- SocketException <$> peek errPtr
              -- On Windows, the second connect fails with `eIsConnected`.
              -- In our case this is not an error condition - other errors are.
              when (err' /= eIsConnected) (throwIO err')
          else throwIO err

-- | Bind a socket to an address.
--
--   - Calling `bind` on a `close`d socket throws `eBadFileDescriptor` even if the former file descriptor has been reassigned.
--   - It is assumed that `bind` never blocks and therefore `eInProgress`, `eAlready` and `eInterrupted` don't occur.
--     This assumption is supported by the fact that the Linux manpage doesn't mention any of these errors,
--     the Posix manpage doesn't mention the last one and even MacOS' implementation will never
--     fail with any of these when the socket is configured non-blocking as
--     [argued here](http://stackoverflow.com/a/14485305).
--   - This operation throws `SocketException`s. Consult your @man@ page for
--     details and specific @errno@s.
bind :: (Family f, Storable (SocketAddress f)) => Socket f t p -> SocketAddress f -> IO ()
bind (Socket mfd) addr =
  alloca $ \addrPtr-> alloca $ \errPtr-> do
    poke addrPtr addr
    withMVar mfd $ \fd-> do
      i <- c_bind fd addrPtr (fromIntegral $ sizeOf addr) errPtr
      when (i /= 0) (SocketException <$> peek errPtr >>= throwIO)

-- | Starts listening and queueing connection requests on a connection-mode
--   socket.
--
--   - Calling `listen` on a `close`d socket throws `eBadFileDescriptor` even if the former
--     file descriptor has been reassigned.
--   - The second parameter is called /backlog/ and sets a limit on how many
--     unaccepted connections the socket implementation shall queue. A value
--     of @0@ leaves the decision to the implementation.
--   - This operation throws `SocketException`s. Consult your @man@ page for
--     details and specific @errno@s.
listen :: Socket f t p -> Int -> IO ()
listen (Socket ms) backlog =
  withMVar ms $ \s-> alloca $ \errPtr-> do
    i <- c_listen s (fromIntegral backlog) errPtr
    when (i /= 0) (SocketException <$> peek errPtr >>= throwIO)

-- | Accept a new connection.
--
--   - Calling `accept` on a `close`d socket throws `eBadFileDescriptor` even if the former
--     file descriptor has been reassigned.
--   - This operation configures the new socket non-blocking (TODO: use `accept4` if available).
--   - This operation sets up a finalizer for the new socket that automatically
--     closes the new socket when the garbage collection decides to collect it.
--     This is just a fail-safe. You might still run out of file descriptors as
--     there's no guarantee about when the finalizer is run. You're advised to
--     manually `close` the socket when it's no longer needed.
--   - This operation throws `SocketException`s. Consult your @man@ page for
--     details and specific @errno@s.
--   - This operation catches `eAgain`, `eWouldBlock` and `eInterrupted` internally
--     and retries automatically.
accept :: (Family f, Storable (SocketAddress f)) => Socket f t p -> IO (Socket f t p, SocketAddress f)
accept s@(Socket mfd) = accept'
  where
    accept' :: forall f t p. (Family f, Storable (SocketAddress f)) => IO (Socket f t p, SocketAddress f)
    accept' = do
      -- Allocate local (!) memory for the address.
      alloca $ \addrPtr-> do
        alloca $ \addrPtrLen-> alloca $ \errPtr-> do
          poke addrPtrLen (fromIntegral $ sizeOf (undefined :: SocketAddress f))
          ( fix $ \again iteration-> do
              -- We mask asynchronous exceptions during this critical section.
              ews <- withMVar mfd $ \fd-> do
                when (fd < 0) (throwIO eBadFileDescriptor)
                bracketOnError
                  ( c_accept fd addrPtr addrPtrLen errPtr )
                  ( \ft-> when (ft >= 0) $ alloca $ void . c_close ft )
                  ( \ft-> if ft < 0
                    then do
                      err <- SocketException <$> peek errPtr
                      unless (err == eWouldBlock || err == eAgain) (throwIO err)
                      return Nothing
                    else do
                      addr <- peek addrPtr :: IO (SocketAddress f)
                      -- newMVar is guaranteed to be not interruptible.
                      mft <- newMVar ft
                      -- Register a finalizer on the new socket.
                      _ <- mkWeakMVar mft (close (Socket mft `asTypeOf` s))
                      return $ Just (Socket mft, addr)
                  )
              -- If ews is Left we got EAGAIN or EWOULDBLOCK and retry after the next event.
              case ews of
                Just sa -> return sa
                Nothing -> unsafeSocketWaitRead mfd iteration >> (again $! iteration + 1)
            ) 0 -- This is the initial iteration value.

-- | Send a message on a connected socket.
--
--   - Calling `send` on a `close`d socket throws `eBadFileDescriptor` even if the former
--     file descriptor has been reassigned.
--   - The operation returns the number of bytes sent. On `Datagram` and
--     `SequentialPacket` sockets certain assurances on atomicity exist and `eAgain` or
--     `eWouldBlock` are returned until the whole message would fit
--     into the send buffer.
--   - This operation throws `SocketException`s. Consult @man 3p send@ for
--     details and specific @errno@s.
--   - `eAgain`, `eWouldBlock` and `eInterrupted` and handled internally and won't
--     be thrown. For performance reasons the operation first tries a write
--     on the socket and then waits when it got `eAgain` or `eWouldBlock`.
send :: Socket f t p -> BS.ByteString -> MessageFlags -> IO Int
send s bs flags = do
  bytesSent <- BS.unsafeUseAsCStringLen bs $ \(bufPtr,bufSize)->
    unsafeSend s (castPtr bufPtr) (fromIntegral bufSize) flags
  return (fromIntegral bytesSent)

-- | Like `send`, but allows to specify a destination address.
sendTo ::(Family f, Storable (SocketAddress f)) => Socket f t p -> BS.ByteString -> MessageFlags -> SocketAddress f -> IO Int
sendTo s bs flags addr = do
  bytesSent <- alloca $ \addrPtr-> do
    poke addrPtr addr
    BS.unsafeUseAsCStringLen bs $ \(bufPtr,bufSize)->
      unsafeSendTo s bufPtr (fromIntegral bufSize) flags addrPtr (fromIntegral $ sizeOf addr)
  return (fromIntegral bytesSent)

-- | Receive a message on a connected socket.
--
--   - Calling `receive` on a `close`d socket throws `eBadFileDescriptor` even if the former file descriptor has been reassigned.
--   - The operation takes a buffer size in bytes a first parameter which
--     limits the maximum length of the returned `Data.ByteString.ByteString`.
--   - This operation throws `SocketException`s. Consult @man 3p receive@ for
--     details and specific @errno@s.
--   - `eAgain`, `eWouldBlock` and `eInterrupted` and handled internally and won't be thrown.
--     For performance reasons the operation first tries a read
--     on the socket and then waits when it got `eAgain` or `eWouldBlock`.
receive :: Socket f t p -> Int -> MessageFlags -> IO BS.ByteString
receive s bufSize flags =
  bracketOnError
    ( mallocBytes bufSize )
    (\bufPtr-> free bufPtr )
    (\bufPtr-> do
        bytesReceived <- unsafeReceive s bufPtr (fromIntegral bufSize) flags
        BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bytesReceived)
    )

-- | Like `receive`, but additionally yields the peer address.
receiveFrom :: (Family f, Storable (SocketAddress f)) => Socket f t p -> Int -> MessageFlags -> IO (BS.ByteString, SocketAddress f)
receiveFrom = receiveFrom'
  where
    receiveFrom' :: forall f t p. (Family f, Storable (SocketAddress f)) => Socket f t p -> Int -> MessageFlags -> IO (BS.ByteString, SocketAddress f)
    receiveFrom' s bufSize flags = do
      alloca $ \addrPtr-> do
        alloca $ \addrSizePtr-> do
          poke addrSizePtr (fromIntegral $ sizeOf (undefined :: SocketAddress f))
          bracketOnError
            ( mallocBytes bufSize )
            (\bufPtr-> free bufPtr )
            (\bufPtr-> do
                bytesReceived <- unsafeReceiveFrom s bufPtr (fromIntegral bufSize) flags addrPtr addrSizePtr
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
--     Threads that perform operations other than `close` on this socket will fail with `eBadFileDescriptor`
--     after the socket has been closed (`close` replaces the
--     `System.Posix.Types.Fd` in the `Control.Concurrent.MVar.MVar` with @-1@
--     to reliably avoid use-after-free situations).
--   - This operation potentially throws `SocketException`s (only @EIO@ is
--     documented). `eInterrupted` is catched internally and retried automatically, so won't be thrown.
close :: Socket f t p -> IO ()
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
        ( const $ alloca $ \errPtr-> fix $ \retry-> do
            i <- c_close fd errPtr
            when (i /= 0) $ do
              err <- SocketException <$> peek errPtr
              when (err /= eInterrupted) (throwIO err)
              retry
        ) fd
      -- When we arrive here, no exception has been thrown and the descriptor has been closed.
      -- We put an invalid file descriptor into the MVar.
      return (-1)
