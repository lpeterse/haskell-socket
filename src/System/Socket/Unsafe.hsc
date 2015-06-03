module System.Socket.Unsafe (
  -- * unsafeSend
    unsafeSend
  -- * unsafeSendMsg
  , unsafeSendMsg
  -- * unsafeSendTo
  , unsafeSendTo
  -- * unsafeRecv
  , unsafeRecv
  -- * unsafeRecvMsg
  , unsafeRecvMsg
  -- * unsafeRecvFrom
  , unsafeRecvFrom
  -- * unsafeUseAsMsgPtr
  , unsafeUseAsMsgPtr
  ) where

import Data.Function
import Data.Monoid

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS

import Control.Monad
import Control.Exception
import Control.Concurrent.MVar

import Foreign.C.Error
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import System.Socket.Internal.Socket
import System.Socket.Internal.Event
import System.Socket.Internal.FFI
import System.Socket.Internal.Exception
import System.Socket.Internal.Msg
import System.Socket.Internal.MsgFlags
import System.Socket.Family
import System.Socket.Type
import System.Socket.Protocol

#include "sys/socket.h"

unsafeSend :: Socket a t p -> Ptr a -> CSize -> MsgFlags -> IO CInt
unsafeSend (Socket mfd) bufPtr bufSize flags = do
  fix $ \again-> do
    ewb <- withMVar mfd $ \fd-> do
      when (fd < 0) $ do
        throwIO (SocketException eBADF)
      fix $ \retry-> do
        i <- c_send fd bufPtr bufSize (flags `mappend` msgNOSIGNAL)
        if (i < 0) then do
          e <- getErrno
          if e == eWOULDBLOCK || e == eAGAIN
            then threadWaitWrite' fd >>= return . Left
          else if e == eINTR
            then retry
            else throwIO (SocketException e)
        -- Send succeeded. Return the bytes send.
        else return (Right i)
    case ewb of
      Left  wait          -> wait >> again
      Right bytesSent     -> return bytesSent

unsafeSendTo :: Socket f t p -> Ptr b -> CSize -> MsgFlags -> Ptr (Address f) -> CInt -> IO CInt
unsafeSendTo (Socket mfd) bufPtr bufSize flags addrPtr addrSize = do
  fix $ \again-> do
    ewb <- withMVar mfd $ \fd-> do
      when (fd < 0) $ do
        throwIO (SocketException eBADF)
      fix $ \retry-> do
        i <- c_sendto fd bufPtr (fromIntegral bufSize) (flags `mappend` msgNOSIGNAL) addrPtr addrSize
        if (i < 0) then do
          e <- getErrno
          if e == eWOULDBLOCK || e == eAGAIN
            then threadWaitWrite' fd >>= return . Left
          else if e == eINTR
            then retry
            else throwIO (SocketException e)
        -- Send succeeded. Return the bytes send.
        else return (Right i)
    case ewb of
      Left  wait          -> wait >> again
      Right bytesSent     -> return (fromIntegral bytesSent)

unsafeSendMsg :: Socket a t p -> Ptr (Msg a t p) -> MsgFlags -> IO CInt
unsafeSendMsg (Socket mfd) msghdrPtr flags = do
  fix $ \again-> do
    ewb <- withMVar mfd $ \fd-> do
      when (fd < 0) $ do
        throwIO (SocketException eBADF)
      fix $ \retry-> do
        i <- c_sendmsg fd msghdrPtr (flags `mappend` msgNOSIGNAL)
        if (i < 0) then do
          e <- getErrno
          if e == eWOULDBLOCK || e == eAGAIN
            then threadWaitWrite' fd >>= return . Left
          else if e == eINTR
            then retry
            else throwIO (SocketException e)
        -- Send succeeded. Return the bytes send.
        else return (Right i)
    case ewb of
      Left  wait          -> wait >> again
      Right bytesSent     -> return bytesSent

unsafeRecv :: Socket a t p -> Ptr b -> CSize -> MsgFlags -> IO CInt
unsafeRecv (Socket mfd) bufPtr bufSize flags =
  fix $ \again-> do
    ewb <- withMVar mfd $ \fd-> do
        when (fd < 0) $ do
          throwIO (SocketException eBADF)
        fix $ \retry-> do
          i <- c_recv fd bufPtr bufSize flags
          if (i < 0) then do
            e <- getErrno
            if e == eWOULDBLOCK || e == eAGAIN then do
              threadWaitRead' fd >>= return . Left
            else if e == eINTR
              then retry
              else throwIO (SocketException e)
          else return (Right i)
    case ewb of
      Left  wait          -> wait >> again
      Right bytesReceived -> return bytesReceived

unsafeRecvMsg :: Socket a t p -> Ptr (Msg a t p) -> MsgFlags -> IO CInt
unsafeRecvMsg (Socket mfd) msgPtr flags =
  fix $ \again-> do
    ewb <- withMVar mfd $ \fd-> do
        when (fd < 0) $ do
          throwIO (SocketException eBADF)
        fix $ \retry-> do
          i <- c_recvmsg fd msgPtr flags
          if (i < 0) then do
            e <- getErrno
            if e == eWOULDBLOCK || e == eAGAIN then do
              threadWaitRead' fd >>= return . Left
            else if e == eINTR
              then retry
              else throwIO (SocketException e)
          else return (Right i)
    case ewb of
      Left  wait          -> wait >> again
      Right bytesReceived -> return bytesReceived

unsafeRecvFrom :: Socket f t p -> Ptr b -> CSize -> MsgFlags -> Ptr (Address f) -> Ptr CInt -> IO CInt
unsafeRecvFrom (Socket mfd) bufPtr bufSize flags addrPtr addrSizePtr = do
  fix $ \again-> do
    ewb <- withMVar mfd $ \fd-> do
        when (fd < 0) $ do
          throwIO (SocketException eBADF)
        fix $ \retry-> do
          i <- c_recvfrom fd bufPtr bufSize flags addrPtr addrSizePtr
          if (i < 0) then do
            e <- getErrno
            if e == eWOULDBLOCK || e == eAGAIN then do
              threadWaitRead' fd >>= return . Left
            else if e == eINTR
              then retry
              else throwIO (SocketException e)
          else return (Right i)
    case ewb of
      Left  wait          -> wait >> again
      Right bytesReceived -> return (fromIntegral bytesReceived)

unsafePokeByteStringToIoVec :: Ptr IoVec -> BS.ByteString -> IO ()
unsafePokeByteStringToIoVec iovecPtr bs = do
  c_memset iovecPtr 0 (#const sizeof(struct iovec))
  BS.unsafeUseAsCStringLen bs $ \(bufPtr, bufLen)-> do
    poke (iov_base iovecPtr) bufPtr
    poke (iov_len  iovecPtr) (fromIntegral bufLen)
  where
    iov_base = (#ptr struct iovec, iov_base) :: Ptr IoVec -> Ptr CString
    iov_len  = (#ptr struct iovec, iov_len)  :: Ptr IoVec -> Ptr CSize

-- | FIXME: empty ByteString causes EINVAL
unsafeUseAsMsgPtr :: LBS.ByteString -> (Ptr (Msg a t p) -> IO o) -> IO o
unsafeUseAsMsgPtr bytestring performWith = do
  allocaBytes (#const sizeof(struct msghdr)) $ \msgHdrPtr-> do
    allocaBytes (chunkCount * (#const sizeof(struct iovec))) $ \iovArrPtr-> do
      c_memset msgHdrPtr 0 (#const sizeof(struct msghdr))
      poke (msg_iov    msgHdrPtr) iovArrPtr
      poke (msg_iovlen msgHdrPtr) (fromIntegral chunkCount)
      LBS.foldrChunks 
        (\bs action-> \pos-> do
          unsafePokeByteStringToIoVec (iovArrPtr `plusPtr` (pos * (#const sizeof(struct iovec)))) bs
          action (pos + 1)
        ) (const $ return ()) bytestring 0
      performWith msgHdrPtr
  where
    chunkCount     = length (LBS.toChunks bytestring)
--    msg_name       = (#ptr struct msghdr, msg_name)   :: Ptr (Msg a t p) -> Ptr (Ptr a)
--    msg_namelen    = (#ptr struct msghdr, msg_namelen):: Ptr (Msg a t p) -> Ptr CInt
    msg_iov        = (#ptr struct msghdr, msg_iov)    :: Ptr (Msg a t p) -> Ptr (Ptr IoVec)
    msg_iovlen     = (#ptr struct msghdr, msg_iovlen) :: Ptr (Msg a t p) -> Ptr CSize
    msg_flags      = (#ptr struct msghdr, msg_flags)  :: Ptr (Msg a t p) -> Ptr CInt