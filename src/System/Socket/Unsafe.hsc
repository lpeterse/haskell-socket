module System.Socket.Unsafe (
  -- * tryWaitAndRetry
    tryWaitAndRetry
  -- * unsafeSend
  , unsafeSend
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
import System.Socket.Family

import System.Posix.Types (Fd)

#include "hs_socket.h"

unsafeSend :: Socket a t p -> Ptr a -> CSize -> MsgFlags -> IO CInt
unsafeSend s bufPtr bufSize flags = do
  tryWaitAndRetry s threadWaitWrite' (\fd-> c_send fd bufPtr bufSize (flags `mappend` msgNOSIGNAL) )

unsafeSendTo :: Socket f t p -> Ptr b -> CSize -> MsgFlags -> Ptr (Address f) -> CInt -> IO CInt
unsafeSendTo s bufPtr bufSize flags addrPtr addrSize = do
  tryWaitAndRetry s threadWaitWrite' (\fd-> c_sendto fd bufPtr (fromIntegral bufSize) (flags `mappend` msgNOSIGNAL) addrPtr addrSize)

unsafeSendMsg :: Socket a t p -> Ptr (Msg a t p) -> MsgFlags -> IO CInt
unsafeSendMsg s msghdrPtr flags = do
  tryWaitAndRetry s threadWaitWrite' (\fd-> c_sendmsg fd msghdrPtr (flags `mappend` msgNOSIGNAL))

unsafeRecv :: Socket a t p -> Ptr b -> CSize -> MsgFlags -> IO CInt
unsafeRecv s bufPtr bufSize flags =
  tryWaitAndRetry s threadWaitRead' (\fd-> c_recv fd bufPtr bufSize flags)

unsafeRecvMsg :: Socket a t p -> Ptr (Msg a t p) -> MsgFlags -> IO CInt
unsafeRecvMsg s msgPtr flags =
  tryWaitAndRetry s threadWaitRead' (\fd-> c_recvmsg fd msgPtr flags)

unsafeRecvFrom :: Socket f t p -> Ptr b -> CSize -> MsgFlags -> Ptr (Address f) -> Ptr CInt -> IO CInt
unsafeRecvFrom s bufPtr bufSize flags addrPtr addrSizePtr = do
  tryWaitAndRetry s threadWaitRead' (\fd-> c_recvfrom fd bufPtr bufSize flags addrPtr addrSizePtr)

tryWaitAndRetry :: Socket f t p -> (Fd -> IO (IO ())) -> (Fd -> IO CInt) -> IO CInt
tryWaitAndRetry (Socket mfd) getWaitAction action = do
  fix $ \again-> do
    ewr <- withMVar mfd $ \fd-> do
        when (fd < 0) $ do
          throwIO eBADF
        fix $ \retry-> do
          i <- action fd
          if (i < 0) then do
            e <- c_get_last_socket_error
            if e == eWOULDBLOCK || e == eAGAIN then do
              getWaitAction fd >>= return . Left
            else if e == eINTR
              then retry
              else throwIO e
          else return (Right i)
    case ewr of
      Left  wait   -> wait >> again
      Right result -> return result

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
    msg_iov        = (#ptr struct msghdr, msg_iov)    :: Ptr (Msg a t p) -> Ptr (Ptr IoVec)
    msg_iovlen     = (#ptr struct msghdr, msg_iovlen) :: Ptr (Msg a t p) -> Ptr CSize
