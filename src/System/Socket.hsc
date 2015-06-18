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
-- This starts a TCP server on localhost, sends @"Hello world!"@ to
-- connecting peers and closes the connection immediately.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import System.Socket
-- > import System.Socket.Family.Inet (inaddrLOOPBACK)
-- > import Data.Monoid
-- > import Data.ByteString
-- > import Control.Monad
-- > import Control.Concurrent
-- > import Control.Exception
-- >
-- > main :: IO ()
-- > main = do
-- >   s <- socket :: IO (Socket Inet STREAM TCP)
-- >   setSockOpt s (SO_REUSEADDR True)
-- >   bind s (SocketAddressInet 8080 inaddrLOOPBACK)
-- >   listen s 5
-- >   forever $ do
-- >     (peer,addr) <- accept s
-- >     forkIO $ do
-- >       sendAll peer "Hello world!" mempty `finally` close peer
--
-- This downloads the [Haskell website](http://www.haskell.org) and shows how to
-- handle exceptions. Note the use of IPv4-mapped `Inet6` addresses: This will work
-- even if you don't have IPv6 connectivity yet and is the preferred method
-- when writing new applications.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- > 
-- > import Data.Monoid
-- > import Data.ByteString.Lazy as B
-- > import System.Socket
-- > 
-- > main :: IO ()
-- > main = do
-- >   withConnectedSocket "www.haskell.org" "80" (aiALL `mappend` aiV4MAPPED) $ \sock-> do
-- >     let _ = sock :: Socket Inet6 STREAM TCP
-- >     sendAll sock "GET / HTTP/1.0\r\nHost: www.haskell.org\r\n\r\n" mempty
-- >     x <- receiveAll sock (1024*1024*1024) mempty
-- >     B.putStr x
-----------------------------------------------------------------------------
module System.Socket (
  -- * Name Resolution
    AddressInfo (..)
  -- ** getAddressInfo
  , GetAddressInfo (..)
  -- ** getNameInfo
  , GetNameInfo (..)
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
  -- * Convenience Operations
  -- ** withConnectedSocket
  , withConnectedSocket
  -- ** sendAll
  , sendAll
  -- ** receiveAll
  , receiveAll
  -- * Sockets
  , Socket (..)
  -- ** Families
  , Family (..)
  -- *** Inet
  , Inet
  -- *** Inet6
  , Inet6
  -- ** Types
  , Type (..)
  -- *** DGRAM
  , DGRAM
  -- *** RAW
  , RAW
    -- *** SEQPACKET
  , SEQPACKET
  -- *** STREAM
  , STREAM
  -- ** Protocols
  , Protocol  (..)
  -- *** UDP
  , UDP
  -- *** TCP
  , TCP
  -- * Exceptions
  -- ** SocketException
  , module System.Socket.Internal.Exception
  -- ** AddressInfoException
  , AddressInfoException (..)
  , gaiStrerror
  , eaiAGAIN
  , eaiBADFLAGS
  , eaiFAIL
  , eaiFAMILY
  , eaiMEMORY
  , eaiNONAME
  , eaiSOCKTYPE
  , eaiSERVICE
  , eaiSYSTEM
  -- * Socket Options
  -- ** getSockOpt
  , GetSockOpt (..)
  -- ** setSockOpt
  , SetSockOpt (..)
  , SO_ERROR (..)
  , SO_REUSEADDR (..)
  -- * Flags
  -- ** MsgFlags
  , MsgFlags (..)
  , msgEOR
  , msgNOSIGNAL
  , msgOOB
  , msgWAITALL
  -- ** AddressInfoFlags
  , AddressInfoFlags (..)
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
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar

import Data.Function
import Data.Monoid
import Data.Int
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.Lazy as LBS

import GHC.Conc (closeFdWith)

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.Socket.Unsafe

import System.Socket.Internal.Socket
import System.Socket.Internal.Exception
import System.Socket.Internal.Msg
import System.Socket.Internal.AddressInfo
import System.Socket.Internal.Platform

import System.Socket.Family
import System.Socket.Family.Inet
import System.Socket.Family.Inet6

import System.Socket.Type
import System.Socket.Type.DGRAM
import System.Socket.Type.RAW
import System.Socket.Type.SEQPACKET
import System.Socket.Type.STREAM

import System.Socket.Protocol
import System.Socket.Protocol.UDP
import System.Socket.Protocol.TCP

#include "hs_socket.h"

-- | Creates a new socket.
--
--   Whereas the underlying POSIX socket operation takes 3 parameters, this library
--   encodes this information in the type variables. This rules out several
--   kinds of errors and escpecially simplifies the handling of addresses (by using
--   associated type families). Examples:
--
--   > -- create a IPv4-UDP-datagram socket
--   > sock <- socket :: IO (Socket Inet DGRAM UDP)
--   > -- create a IPv6-TCP-streaming socket
--   > sock6 <- socket :: IO (Socket Inet6 STREAM TCP)
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
--       > result <- bracket (socket :: IO (Socket Inet6 STREAM TCP)) close $ \sock-> do
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
   socket'  = do
     bracketOnError
       -- Try to acquire the socket resource. This part has exceptions masked.
       ( c_socket (familyNumber (undefined :: f)) (typeNumber (undefined :: t)) (protocolNumber (undefined :: p)) )
       -- On failure after the c_socket call we try to close the socket to not leak file descriptors.
       -- If closing fails we cannot really do something about it. We tried at least.
       -- This part has exceptions masked as well. c_close is an unsafe FFI call.
       ( \fd-> when (fd >= 0) (c_close fd >> return ()) )
       -- If an exception is raised, it is reraised after the socket has been closed.
       -- This part has async exceptions unmasked (via restore).
       ( \fd-> if fd < 0 then do
                c_get_last_socket_error >>= throwIO
              else do
                -- setNonBlockingFD calls c_fcntl_write which is an unsafe FFI call.
                i <- c_setnonblocking fd
                if i < 0 then do
                  c_get_last_socket_error >>= throwIO
                else do
                  mfd <- newMVar fd
                  let s = Socket mfd
                  _ <- mkWeakMVar mfd (close s)
                  return s
       )

-- | Connects to an remote address.
--
--   - Calling `connect` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - This operation returns as soon as a connection has been established (as
--     if the socket were blocking). The connection attempt has either failed or
--     succeeded after this operation threw an exception or returned.
--   - The operation might throw `SocketException`s. Due to implementation quirks
--     the socket should be considered in an undefined state when this operation
--     failed. It should be closed then.
--   - Also see [these considerations](http://cr.yp.to/docs/connect.html) on
--     the problems with connecting non-blocking sockets.
connect :: Family f => Socket f t p -> SocketAddress f -> IO ()
connect (Socket mfd) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    let addrLen = fromIntegral (sizeOf addr)
    mwait <- withMVar mfd $ \fd-> do
      when (fd < 0) (throwIO eBADF)
      -- The actual connection attempt.
      i <- c_connect fd addrPtr addrLen
      -- On non-blocking sockets we expect to get EINPROGRESS or EWOULDBLOCK.
      if i < 0 then do
        e <- c_get_last_socket_error
        if e == eINPROGRESS || e == eWOULDBLOCK || e == eINTR then do
          -- The manpage says that in this case the connection
          -- shall be established asynchronously and one is
          -- supposed to wait.
          wait <- socketWaitWrite' fd 0
          return (Just wait)
        else do
          throwIO e
      else do
        -- This should not be the case on non-blocking socket, but better safe than sorry.
        return Nothing
    case mwait of
      Nothing -> do
        -- The connection could be established synchronously. Nothing else to do.
        return ()
      Just wait -> do
        -- This either waits or does nothing.
        wait
        -- By here we don't know anything about the current connection status.
        -- It might either have succeeded, failed or is still undecided.
        -- The approach is to try a second connection attempt, because its error
        -- codes allow us to distinguish all three potential states.
        ( fix $ \again iteration-> do
            mwait' <- withMVar mfd $ \fd-> do
              i <- c_connect fd addrPtr addrLen
              if i < 0 then do
                e <- c_get_last_socket_error
                if e == eISCONN then do
                  -- This is what we want. The connection is established.
                  return Nothing
                else if e == eALREADY then do
                  -- The previous connection attempt is still pending.
                  Just <$> socketWaitWrite' fd iteration
                else do
                  -- The previous connection failed (results in EINPROGRESS or
                  -- EWOULBLOCK here) or something else is wrong.
                  -- We throw eTIMEDOUT here as we don't know and will never
                  -- know the exact reason (better suggestions appreciated).
                  throwIO eTIMEDOUT
              else do
                -- This means the last connection attempt succeeded immediately.
                -- Linux does this when connecting to the same address when the
                -- socketWaitWrite' call signals writeability.
                return Nothing
            case mwait' of
              Nothing    -> do
                return ()
              Just wait' -> do
                wait'
                again $! iteration + 1
         ) 1

-- | Bind a socket to an address.
--
--   - Calling `bind` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - It is assumed that `c_bind` never blocks and therefore @EINPROGRESS@, @EALREADY@ and @EINTR@ don't occur.
--     This assumption is supported by the fact that the Linux manpage doesn't mention any of these errors,
--     the Posix manpage doesn't mention the last one and even MacOS' implementation will never
--     fail with any of these when the socket is configured non-blocking as
--     [argued here](http://stackoverflow.com/a/14485305).
--   - This operation throws `SocketException`s. Consult your @man@ page for
--     details and specific @errno@s.
bind :: (Family f) => Socket f t p -> SocketAddress f -> IO ()
bind (Socket mfd) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    withMVar mfd $ \fd-> do
      i <- c_bind fd addrPtr (fromIntegral $ sizeOf addr)
      if i < 0
        then c_get_last_socket_error >>= throwIO
        else return ()

-- | Starts listening and queueing connection requests on a connection-mode
--   socket.
--
--   - Calling `listen` on a `close`d socket throws @EBADF@ even if the former
--     file descriptor has been reassigned.
--   - The second parameter is called /backlog/ and sets a limit on how many
--     unaccepted connections the socket implementation shall queue. A value
--     of @0@ leaves the decision to the implementation.
--   - This operation throws `SocketException`s. Consult your @man@ page for
--     details and specific @errno@s.
listen :: Socket f t p -> Int -> IO ()
listen (Socket ms) backlog = do
  i <- withMVar ms $ \s-> do
    c_listen s (fromIntegral backlog)
  if i < 0 then do
    c_get_last_socket_error >>= throwIO
  else do
    return ()

-- | Accept a new connection.
--
--   - Calling `accept` on a `close`d socket throws @EBADF@ even if the former
--     file descriptor has been reassigned.
--   - This operation configures the new socket non-blocking (TODO: use `accept4` if available).
--   - This operation sets up a finalizer for the new socket that automatically
--     closes the new socket when the garbage collection decides to collect it.
--     This is just a fail-safe. You might still run out of file descriptors as
--     there's no guarantee about when the finalizer is run. You're advised to
--     manually `close` the socket when it's no longer needed.
--   - This operation throws `SocketException`s. Consult your @man@ page for
--     details and specific @errno@s.
--   - This operation catches @EAGAIN@, @EWOULDBLOCK@ and @EINTR@ internally
--     and retries automatically.
accept :: (Family f) => Socket f t p -> IO (Socket f t p, SocketAddress f)
accept s@(Socket mfd) = accept'
  where
    accept' :: forall f t p. (Family f) => IO (Socket f t p, SocketAddress f)
    accept' = do
      -- Allocate local (!) memory for the address.
      alloca $ \addrPtr-> do
        alloca $ \addrPtrLen-> do
          poke addrPtrLen (fromIntegral $ sizeOf (undefined :: SocketAddress f))
          ( fix $ \again iteration-> do
              -- We mask asynchronous exceptions during this critical section.
              ews <- withMVarMasked mfd $ \fd-> do
                fix $ \retry-> do
                  ft <- c_accept fd addrPtr addrPtrLen
                  if ft < 0 then do
                    e <- c_get_last_socket_error
                    if e == eWOULDBLOCK || e == eAGAIN
                      then do
                        socketWaitRead' fd iteration >>= return . Left
                      else if e == eINTR
                        -- On EINTR it is good practice to just retry.
                        then retry
                        else throwIO e
                  -- This is the critical section: We got a valid descriptor we have not yet returned.
                  else do 
                    i <- c_setnonblocking ft
                    if i < 0 then do
                      c_get_last_socket_error >>= throwIO
                    else do
                      -- This peek operation might be a little expensive, but I don't see an alternative.
                      addr <- peek addrPtr :: IO (SocketAddress f)
                      -- newMVar is guaranteed to be not interruptible.
                      mft <- newMVar ft
                      -- Register a finalizer on the new socket.
                      _ <- mkWeakMVar mft (close (Socket mft `asTypeOf` s))
                      return (Right (Socket mft, addr))
              -- If ews is Left we got EAGAIN or EWOULDBLOCK and retry after the next event.
              case ews of
                Left  wait -> wait >> (again $! iteration + 1)
                Right sock -> return sock
              ) 0 -- This is the initial iteration value.

-- | Send a message on a connected socket.
--
--   - Calling `send` on a `close`d socket throws @EBADF@ even if the former
--     file descriptor has been reassigned.
--   - The operation returns the number of bytes sent. On @DGRAM@ and
--     @SEQPACKET@ sockets certain assurances on atomicity exist and @EAGAIN@ or
--     @EWOULDBLOCK@ are returned until the whole message would fit
--     into the send buffer. 
--   - The flag @MSG_NOSIGNAL@ is set to supress signals which are pointless.
--   - This operation throws `SocketException`s. Consult @man 3p send@ for
--     details and specific @errno@s.
--   - @EAGAIN@, @EWOULDBLOCK@ and @EINTR@ and handled internally and won't
--     be thrown. For performance reasons the operation first tries a write
--     on the socket and then waits when it got @EAGAIN@ or @EWOULDBLOCK@.
send :: Socket f t p -> BS.ByteString -> MsgFlags -> IO Int
send s bs flags = do
  bytesSent <- BS.unsafeUseAsCStringLen bs $ \(bufPtr,bufSize)->
    unsafeSend s (castPtr bufPtr) (fromIntegral bufSize) flags
  return (fromIntegral bytesSent)

-- | Like `send`, but allows for specifying a destination address.
sendTo ::(Family f) => Socket f t p -> BS.ByteString -> MsgFlags -> SocketAddress f -> IO Int
sendTo s bs flags addr = do
  bytesSent <- alloca $ \addrPtr-> do
    poke addrPtr addr
    BS.unsafeUseAsCStringLen bs $ \(bufPtr,bufSize)->
      unsafeSendTo s bufPtr (fromIntegral bufSize) flags addrPtr (fromIntegral $ sizeOf addr)
  return (fromIntegral bytesSent)

-- | Receive a message on a connected socket.
--
--   - Calling `receive` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - The operation takes a buffer size in bytes a first parameter which
--     limits the maximum length of the returned `Data.ByteString.ByteString`.
--   - This operation throws `SocketException`s. Consult @man 3p receive@ for
--     details and specific @errno@s.
--   - @EAGAIN@, @EWOULDBLOCK@ and @EINTR@ and handled internally and won't be thrown.
--     For performance reasons the operation first tries a read
--     on the socket and then waits when it got @EAGAIN@ or @EWOULDBLOCK@.
receive :: Socket f t p -> Int -> MsgFlags -> IO BS.ByteString
receive s bufSize flags =
  bracketOnError
    ( mallocBytes bufSize )
    (\bufPtr-> free bufPtr )
    (\bufPtr-> do
        bytesReceived <- unsafeReceive s bufPtr (fromIntegral bufSize) flags
        BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bytesReceived)
    )

-- | Like `receive`, but additionally yields the peer address.
receiveFrom :: (Family f) => Socket f t p -> Int -> MsgFlags -> IO (BS.ByteString, SocketAddress f)
receiveFrom = receiveFrom'
  where
    receiveFrom' :: forall f t p. (Family f) => Socket f t p -> Int -> MsgFlags -> IO (BS.ByteString, SocketAddress f)
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
--     Threads that perform operations other than `close` on this socket will fail with @EBADF@
--     after the socket has been closed (`close` replaces the 
--     `System.Posix.Types.Fd` in the `Control.Concurrent.MVar.MVar` with @-1@
--     to reliably avoid use-after-free situations).
--   - This operation potentially throws `SocketException`s (only @EIO@ is
--     documented). @EINTR@ is catched internally and retried automatically, so won't be thrown.
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
        ( const $ fix $ \retry-> do
            i <- c_close fd
            if i < 0 then do
              e <- c_get_last_socket_error
              if e == eINTR 
                then retry
                else throwIO e
            else return ()
        ) fd
      -- When we arrive here, no exception has been thrown and the descriptor has been closed.
      -- We put an invalid file descriptor into the MVar.
      return (-1)

-------------------------------------------------------------------------------
-- Convenience Operations
-------------------------------------------------------------------------------

-- | Like `send`, but operates on lazy `Data.ByteString.Lazy.ByteString`s and 
--   continues until all data has been sent or an exception occured.
sendAll ::Socket f STREAM p -> LBS.ByteString -> MsgFlags -> IO ()
sendAll s lbs flags =
  LBS.foldlChunks
    (\x bs-> x >> sendAll' bs
    ) (return ()) lbs
  where
    sendAll' bs = do
      sent <- send s bs flags
      when (sent < BS.length bs) $ sendAll' (BS.drop sent bs)

-- | Like `receive`, but operates on lazy `Data.ByteString.Lazy.ByteString`s and
--   continues until either an empty part has been received (peer closed
--   the connection) or given buffer limit has been exceeded or an
--   exception occured.
--
--   - The `Int` parameter is a soft limit on how many bytes to receive.
--     Collection is stopped if the limit has been exceeded. The result might
--     be up to one internal buffer size longer than the given limit.
--     If the returned `Data.ByteString.Lazy.ByteString`s length is lower or
--     eqal than the limit, the data has not been truncated and the
--     transmission is complete.
receiveAll :: Socket f STREAM p -> Int64 -> MsgFlags -> IO LBS.ByteString
receiveAll sock maxLen flags = collect 0 mempty
  where
    collect len accum
      | len > maxLen = do
          build accum
      | otherwise = do
          bs <- receive sock BB.smallChunkSize flags
          if BS.null bs then do
            build accum
          else do
            collect (len + fromIntegral (BS.length bs))
                 $! (accum `mappend` BB.byteString bs)
    build accum = do
      return (BB.toLazyByteString accum)

-- | Looks up a name and executes an supplied action with a connected socket.
--
-- - The addresses returned by `getAddressInfo` are tried in sequence until a
--   connection has been established or all have been tried.
-- - If `connect` fails on all addresses the exception that occured on the
--   last connection attempt is thrown.
-- - The supplied action is executed at most once with the first established
--   connection.
-- - If the address family is `Inet6`, `IPV6_V6ONLY` is set to `False` which
--   means the other end may be both IPv4 or IPv6.
-- - All sockets created by this operation get closed automatically.
-- - This operation throws `AddressInfoException`s, `SocketException`s and all
--   exceptions that that the supplied action might throw.
--
-- > withConnectedSocket "wwww.haskell.org" "80" (aiALL `mappend` aiV4MAPPED) $ \sock-> do
-- >   let _ = sock :: Socket Inet6 STREAM TCP
-- >   doSomethingWithSocket sock
withConnectedSocket :: forall f t p a.
                 ( GetAddressInfo f, Type t, Protocol p)
                => BS.ByteString
                -> BS.ByteString
                -> AddressInfoFlags
                -> (Socket f t p -> IO a)
                -> IO a
withConnectedSocket host serv flags action = do
  addrs <- getAddressInfo (Just host) (Just serv) flags :: IO [AddressInfo f t p]
  tryAddrs addrs
  where
    tryAddrs :: [AddressInfo f t p] -> IO a
    tryAddrs [] = do
      -- This should not happen.
      throwIO eaiNONAME
    tryAddrs (addr:addrs) = do
      eith <- bracket
        ( socket )
        ( close )
        ( \sock-> do
            configureSocketSpecific sock
            connected <- try (connect sock $ socketAddress addr)
            case connected of
              Left e  -> return (Left (e :: SocketException))
              Right _ -> Right <$> action sock
        )
      case eith of
        Left e ->
          -- Rethrow the last exception if there are no more addresses to try.
          if null addrs
            then throwIO e
            else tryAddrs addrs
        Right a -> do
          return a

    configureSocketSpecific sock = do
      when (familyNumber (undefined :: f) == familyNumber (undefined :: Inet6)) $ do
        setSockOpt sock (IPV6_V6ONLY False)
