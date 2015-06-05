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
-- > import System.Socket.Family.INET (inaddrLOOPBACK)
-- > import Data.ByteString
-- > import Control.Monad
-- > import Control.Concurrent
-- > import Control.Exception
-- >
-- > main :: IO ()
-- > main = do
-- >   s <- socket :: IO (Socket INET STREAM TCP)
-- >   setSockOpt s (SO_REUSEADDR)
-- >   bind s (SockAddrIn 8080 inaddrLOOPBACK)
-- >   listen s 5
-- >   forever $ do
-- >     (peer,addr) <- accept s
-- >     forkIO $ do
-- >       sendAll peer "Hello world!" mempty `finally` close peer
--
-- This downloads the [Haskell website](http://www.haskell.org) and shows how to
-- handle exceptions. Note the use of IPv4-mapped IPv6 addresses: This will work
-- even if you don't have IPv6 connectivity yet and is the preferred method
-- when writing new applications.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- > 
-- > import Control.Monad
-- > import Control.Exception
-- > 
-- > import Data.Function (fix)
-- > import qualified Data.ByteString as BS
-- > 
-- > import System.IO
-- > import System.Exit
-- > import System.Socket
-- > 
-- > main :: IO ()
-- > main = fetch
-- >   `catch` (\e-> do
-- >     hPutStr   stderr "Something failed when resolving the name: "
-- >     hPutStrLn stderr $ show (e :: AddrInfoException)
-- >     exitFailure
-- >   )
-- >   `catch` (\e-> do
-- >     hPutStr   stderr "Something went wrong with the socket: "
-- >     hPutStrLn stderr $ show (e :: SocketException)
-- >     exitFailure
-- >   )
-- > 
-- > fetch :: IO ()
-- > fetch = do
-- >   addrs <- getAddrInfo6 (Just "www.haskell.org") (Just "80") aiV4MAPPED :: IO [AddrInfo INET6 STREAM TCP]
-- >   case addrs of
-- >     (addr:_) ->
-- >       -- always use the `bracket` pattern to reliably release resources!
-- >       bracket
-- >         ( socket :: IO (Socket INET6 STREAM TCP) )
-- >         ( close )
-- >         ( \s-> do connect s (addrAddress addr)
-- >                   sendAll s "GET / HTTP/1.0\r\nHost: www.haskell.org\r\n\r\n" mempty
-- >                   fix $ \recvMore-> do
-- >                     bs <- recv s 4096 mempty
-- >                     BS.putStr bs
-- >                     if BS.length bs == 0 -- an empty string means the peer terminated the connection
-- >                       then exitSuccess
-- >                       else recvMore
-- >          )
-- >     _ -> error "Illegal state: getAddrInfo yields non-empty list or exception."
-----------------------------------------------------------------------------
module System.Socket (
  -- * Name Resolution
    AddrInfo (..)
  -- ** getAddrInfo
  , getAddrInfo
  , getAddrInfo6
  -- ** getNameInfo
  , getNameInfo
  , getNameInfo6
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
  -- ** send, sendV
  , send, sendV
    -- ** sendTo, sendToV
  , sendTo, sendToV
  -- ** recv
  , recv
  -- ** recvFrom
  , recvFrom
  -- ** recvMsg
  , recvMsg
  -- ** close
  , close
  -- * Convenience Operations
  -- ** sendAll, sendAllV
  , sendAll, sendAllV
  -- ** recvRecord
  , recvRecord
  -- * Sockets
  , Socket (..)
  -- ** Families
  , Family (..)
  -- *** INET
  , INET
  , SockAddrIn (..)
  -- *** INET6
  , INET6
  , SockAddrIn6 (..)
  -- *** UNIX
  , UNIX
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
  , aiStrError
  , eaiAGAIN
  , eaiBADFLAGS
  , eaiFAIL
  , eaiFAMILY
  , eaiMEMORY
  , eaiSOCKTYPE
  , eaiSERVICE
  , eaiSYSTEM
  -- * Options
  , GetSockOpt (..)
  , SetSockOpt (..)
  -- ** SO_ACCEPTCONN
  , SO_ACCEPTCONN (..)
    -- ** SO_REUSEADDR
  , SO_REUSEADDR (..)
  -- * Flags
  -- ** MsgFlags
  , MsgFlags (..)
  , msgDONTWAIT
  , msgEOR
  , msgMORE
  , msgNOSIGNAL
  , msgOOB
  , msgTRUNC
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
import Control.Applicative
import Control.Concurrent.MVar

import Data.Bits
import Data.Function
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS

import GHC.Conc (closeFdWith)

import Foreign.C.Error
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.Socket.Unsafe

import System.Socket.Internal.Socket
import System.Socket.Internal.Event
import System.Socket.Internal.FFI
import System.Socket.Internal.Exception
import System.Socket.Internal.Msg
import System.Socket.Internal.MsgFlags
import System.Socket.Internal.AddrInfo

import System.Socket.Family
import System.Socket.Family.UNIX
import System.Socket.Family.INET
import System.Socket.Family.INET6

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
--   Whereas the underlying POSIX socket operation takes 3 parameters, this library
--   encodes this information in the type variables. This rules out several
--   kinds of errors and escpecially simplifies the handling of addresses (by using
--   associated type families). Examples:
--
--   > -- create a IPv4-UDP-datagram socket
--   > sock <- socket :: IO (Socket INET DGRAM UDP)
--   > -- create a IPv6-TCP-streaming socket
--   > sock6 <- socket :: IO (Socket INET6 STREAM TCP)
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
--       > result <- bracket (socket :: IO (Socket INET6 STREAM TCP)) close $ \sock-> do
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
--   - This operation throws `SocketException`s. Consult your @man@ page for
--     details and specific @errno@s.
bind :: (Family f) => Socket f t p -> Address f -> IO ()
bind (Socket mfd) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    withMVar mfd $ \fd-> do
      i <- c_bind fd addrPtr (fromIntegral $ sizeOf addr)
      if i < 0
        then getErrno >>= throwIO . SocketException
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
    getErrno >>= throwIO . SocketException
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
accept :: (Family f) => Socket f t p -> IO (Socket f t p, Address f)
accept s@(Socket mfd) = accept'
  where
    accept' :: forall f t p. (Family f) => IO (Socket f t p, Address f)
    accept' = do
      -- Allocate local (!) memory for the address.
      alloca $ \addrPtr-> do
        alloca $ \addrPtrLen-> do
          poke addrPtrLen (fromIntegral $ sizeOf (undefined :: Address f))
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
                    addr <- peek addrPtr :: IO (Address f)
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
--   - This operation throws `SocketException`s. Consult your @man@ page for
--     details and specific @errno@s.
--   - @EINTR@ and @EINPROGRESS@ get catched internally and won't be thrown as the
--     connection might still be established asynchronously. Expect failure
--     when trying to read or write the socket in this case.
connect :: Family f => Socket f t p -> Address f -> IO ()
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

-- | Like `send`, but uses the @sendmsg@ operation to transmit all
--   chunks of a lazy `Data.ByteString.Lazy.ByteString` with one system call
--   by putting them into several @iovec@s.
sendV :: Socket f t p -> LBS.ByteString -> MsgFlags -> IO Int
sendV s lbs flags = do
  bytesSent <- unsafeUseAsMsgPtr lbs $ \msgPtr-> do
    unsafeSendMsg s msgPtr flags
  return (fromIntegral bytesSent)

-- | Like `send`, but allows for specifying a destination address.
sendTo ::(Family f) => Socket f t p -> BS.ByteString -> MsgFlags -> Address f -> IO Int
sendTo s bs flags addr = do
  bytesSent <- alloca $ \addrPtr-> do
    poke addrPtr addr
    BS.unsafeUseAsCStringLen bs $ \(bufPtr,bufSize)->
      unsafeSendTo s bufPtr (fromIntegral bufSize) flags addrPtr (fromIntegral $ sizeOf addr)
  return (fromIntegral bytesSent)

-- | Like `sendTo`, but uses the @sendmsg@ operation to transmit all
--   chunks of a lazy `Data.ByteString.Lazy.ByteString` with one system call
--   by putting them into several @iovec@s.
sendToV ::(Family f) => Socket f t p -> LBS.ByteString -> MsgFlags -> Address f -> IO Int
sendToV s lbs flags addr = do
  bytesSent <- unsafeUseAsMsgPtr lbs $ \msgPtr-> do
    alloca $ \addrPtr-> do
      poke              addrPtr  addr
      poke (msg_name    msgPtr)  addrPtr
      poke (msg_namelen msgPtr) (fromIntegral $ sizeOf addr)
      unsafeSendMsg s msgPtr flags
  return (fromIntegral bytesSent)
  where
    msg_name       = (#ptr struct msghdr, msg_name)   :: Ptr (Msg f t p) -> Ptr (Ptr a)
    msg_namelen    = (#ptr struct msghdr, msg_namelen):: Ptr (Msg f t p) -> Ptr CInt


-- | Receive a message on a connected socket.
--
--   - Calling `recv` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - The operation takes a buffer size in bytes a first parameter which
--     limits the maximum length of the returned `Data.ByteString.ByteString`.
--   - This operation throws `SocketException`s. Consult @man 3p recv@ for
--     details and specific @errno@s.
--   - @EAGAIN@, @EWOULDBLOCK@ and @EINTR@ and handled internally and won't be thrown.
--     For performance reasons the operation first tries a read
--     on the socket and then waits when it got @EAGAIN@ or @EWOULDBLOCK@.
recv :: Socket f t p -> Int -> MsgFlags -> IO BS.ByteString
recv s bufSize flags =
  bracketOnError
    ( mallocBytes bufSize )
    (\bufPtr-> free bufPtr )
    (\bufPtr-> do
        bytesReceived <- unsafeRecv s bufPtr (fromIntegral bufSize) flags
        BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bytesReceived)
    )

-- | Like `recv`, but additionally yields the peer address.
recvFrom :: (Family f) => Socket f t p -> Int -> MsgFlags -> IO (BS.ByteString, Address f)
recvFrom = recvFrom'
  where
    recvFrom' :: forall f t p. (Family f) => Socket f t p -> Int -> MsgFlags -> IO (BS.ByteString, Address f)
    recvFrom' s bufSize flags = do
      alloca $ \addrPtr-> do
        alloca $ \addrSizePtr-> do
          poke addrSizePtr (fromIntegral $ sizeOf (undefined :: Address f))
          bracketOnError
            ( mallocBytes bufSize )
            (\bufPtr-> free bufPtr )
            (\bufPtr-> do
                bytesReceived <- unsafeRecvFrom s bufPtr (fromIntegral bufSize) flags addrPtr addrSizePtr
                addr <- peek addrPtr
                bs   <- BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bytesReceived)
                return (bs, addr)
            )

-- | Like `recv`, but internally uses @sendmsg@ to retrieve associated `MsgFlags`.
recvMsg :: Socket f t p -> Int -> MsgFlags -> IO (BS.ByteString, MsgFlags)
recvMsg s bufSize flags = do
  allocaBytes (#const sizeof(struct msghdr)) $ \msgPtr-> do
    allocaBytes (#const sizeof(struct iovec)) $ \iovPtr-> do
      c_memset msgPtr 0 (#const sizeof(struct msghdr))
      c_memset iovPtr 0 (#const sizeof(struct iovec))
      poke (msg_iov    msgPtr) iovPtr
      poke (msg_iovlen msgPtr) 1
      -- this is to protect the malloced space before it becomes a ByteString
      bracketOnError
        ( mallocBytes bufSize )
        (\bufPtr-> free bufPtr )
        (\bufPtr-> do
            poke (iov_base iovPtr) bufPtr
            poke (iov_len  iovPtr) (fromIntegral bufSize)
            bytesReceived <- unsafeRecvMsg s msgPtr flags
            lbs    <- BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bytesReceived)
            rflags <- MsgFlags <$> peek (msg_flags msgPtr)
            return (lbs, rflags)
        )
  where
    iov_base       = (#ptr struct iovec, iov_base)    :: Ptr IoVec -> Ptr CString
    iov_len        = (#ptr struct iovec, iov_len)     :: Ptr IoVec -> Ptr CSize
    msg_iov        = (#ptr struct msghdr, msg_iov)    :: Ptr (Msg a t p) -> Ptr (Ptr IoVec)
    msg_iovlen     = (#ptr struct msghdr, msg_iovlen) :: Ptr (Msg a t p) -> Ptr CSize
    msg_flags      = (#ptr struct msghdr, msg_flags)  :: Ptr (Msg a t p) -> Ptr CInt

-- | Receives a record (if supported by the protocol). A record consists of several messages terminated by `msgEOR`.
--
-- - The operation accumulates received chunks to a lazy `Data.ByteString.Lazy.ByteString` and waits in between.
-- - It stops accumulating if the supplied maximum length has been reached, but
--   it continues receiving till the record is terminated. Further data gets
--   dropped and `msgTRUNC` will be set.
-- - The operation returns its result as soon as a chunk with `msgEOR` has been received.
-- - Implementation detail: Every (but the last) chunk of the resulting
--   `Data.ByteString.Lazy.ByteString` is exactly as long as the specified buffer size.
recvRecord :: Socket f STREAM p 
           -> Int -- ^ buffer size for a single chunk
           -> Int -- ^ maximum length of the resulting record (to avoid Denial Of Service by memory exhaustion)
           -> MsgFlags
           -> IO (LBS.ByteString, MsgFlags)
recvRecord s bufLen maxLen sflags = do
  allocaBytes (#const sizeof(struct msghdr)) $ \msgPtr-> do
    allocaBytes (#const sizeof(struct iovec)) $ \iovPtr-> do
      c_memset msgPtr 0 (#const sizeof(struct msghdr))
      c_memset iovPtr 0 (#const sizeof(struct iovec))
      poke (msg_iov    msgPtr) iovPtr
      poke (msg_iovlen msgPtr) 1
      bs     <- doCollect s msgPtr iovPtr 0
      rflags <- peek (msg_flags msgPtr)
      return (bs,rflags)
  where
    iov_base       = (#ptr struct iovec, iov_base)    :: Ptr IoVec -> Ptr CString
    iov_len        = (#ptr struct iovec, iov_len)     :: Ptr IoVec -> Ptr CSize
    msg_iov        = (#ptr struct msghdr, msg_iov)    :: Ptr (Msg a t p) -> Ptr (Ptr IoVec)
    msg_iovlen     = (#ptr struct msghdr, msg_iovlen) :: Ptr (Msg a t p) -> Ptr CSize
    msg_flags      = (#ptr struct msghdr, msg_flags)  :: Ptr (Msg a t p) -> Ptr MsgFlags
    -- | This mallocs memory for a single chunk and reads from the socket.
    --   When the buffer is full the chunk is converted to a ByteString and
    --   the operating proceeds with another chunk until msgEOR has been received.
    doCollect sock msgPtr iovPtr resSize =
      if resSize >= maxLen then do
        doDrop sock msgPtr iovPtr
        return LBS.Empty
      else do
        bs <- bracketOnError
          ( mallocBytes bufLen )
          ( free )
          (\bufPtr-> do
            poke (iov_base iovPtr) bufPtr
            doFillBuf sock msgPtr iovPtr bufPtr 0
          )
        rflags <- peek (msg_flags msgPtr)
        LBS.chunk bs <$> if rflags .&. msgEOR /= mempty then do
          return LBS.Empty
        else do
          doCollect sock msgPtr iovPtr (resSize + bufLen)
    -- | This reads from the socket until a single chunk buffer is full
    --   or the `msgEOR` marker has been received.
    doFillBuf sock msgPtr iovPtr bufPtr bufUsage = do
      poke (iov_base iovPtr) (bufPtr `plusPtr` fromIntegral bufUsage)
      poke (iov_len  iovPtr) (fromIntegral bufLen - bufUsage)
      recvdBytes <- unsafeRecvMsg sock msgPtr sflags
      rflags <- peek (msg_flags msgPtr)
      let bufUsage' = bufUsage + fromIntegral recvdBytes
      if bufUsage' >= fromIntegral bufLen || rflags .&. msgEOR /= mempty
        then doFreezeBuf bufPtr bufUsage'
        else doFillBuf sock msgPtr iovPtr bufPtr bufUsage'
    -- | This takes a `Ptr` to malloced memory and converts it to
    --   a ByteString. The result will a have a finalizer associated
    --   with it and must not be freed manually. The `bracketOnError`
    --   is critical. The memory is either freed because an exception
    --   occured before `doFreezeBuf` was called or afterwards by
    --   the `ByteString` finalizer. `doFreezeBuf` is the last thing
    --   that happens is `doFillBuf`.
    doFreezeBuf bufPtr bufUsage = do
      BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bufUsage)
    -- | This reads and drops data from the socket till
    --   a msgEOR has been received.
    doDrop sock msgPtr iovPtr = do
      -- allocate a local buffer for data we ignore anyway
      allocaBytes bufLen $ \bufPtr-> do
        poke (iov_base iovPtr) bufPtr
        fix $ \again-> do
          _      <- unsafeRecvMsg sock msgPtr sflags
          rflags <- peek (msg_flags msgPtr)
          if rflags .&. msgEOR /= mempty
            then poke (msg_flags msgPtr) (rflags `mappend` msgTRUNC)
            else again

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
              e <- getErrno
              if e == eINTR 
                then retry
                else throwIO (SocketException e)
            else return ()
        ) fd
      -- When we arrive here, no exception has been thrown and the descriptor has been closed.
      -- We put an invalid file descriptor into the MVar.
      return (-1)

-------------------------------------------------------------------------------
-- Convenience Operations
-------------------------------------------------------------------------------

-- | Like `send`, but continues until all data has been sent.
--
--   > sendAll sock buf flags = do
--   >   sent <- send sock buf flags
--   >   when (sent < length buf) $ sendAll sock (drop sent buf) flags
sendAll ::Socket f STREAM p -> BS.ByteString -> MsgFlags -> IO ()
sendAll s bs flags = do
  sent <- send s bs flags
  when (sent < BS.length bs) $ sendAll s (BS.drop sent bs) flags

-- | Like `sendV`, but continues until all data has been sent.
--
--   > sendAllV sock buf flags = do
--   >   sent <- sendV sock buf flags
--   >   when (sent < length buf) $ sendAllV sock (drop sent buf) flags
sendAllV ::Socket f STREAM p -> BS.ByteString -> MsgFlags -> IO ()
sendAllV s bs flags = do
  sent <- send s bs flags
  when (sent < BS.length bs) $ sendAll s (BS.drop sent bs) flags
