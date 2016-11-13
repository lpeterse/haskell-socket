--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Type.Stream (
  -- * Stream
    Stream
  -- ** Specialized send operations
  -- *** sendAll
  , sendAll
  -- *** sendAllLazy
  , sendAllLazy
  -- *** sendAllBuilder
  , sendAllBuilder
  -- ** Specialized receive operations
  -- *** receiveAll
  , receiveAll
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Int
import Data.Word
import Data.Monoid
import Foreign.Ptr
import Foreign.Marshal.Alloc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Internal as BB
import qualified Data.ByteString.Lazy as LBS

import System.Socket
import System.Socket.Unsafe

#include "hs_socket.h"

data Stream

instance Type Stream where
  typeNumber _ = (#const SOCK_STREAM)

-------------------------------------------------------------------------------
-- Convenience Operations
-------------------------------------------------------------------------------

sendAll :: Socket f Stream p -> BS.ByteString -> MessageFlags -> IO ()
sendAll s bs flags = sendAll' bs
  where
    -- The `send` operation returns the bytes actually sent. This might be
    -- less than the ByteString is long in case the kernel buffer is full.
    -- In this case we will truncate the bytes already sent and recursively
    -- try again with the remains.
    sendAll' bs' = do
      sent <-  send s bs' flags
      when (sent < BS.length bs') $ sendAll' (BS.drop sent bs')

-- | Like `sendAll`, but operates on lazy `Data.ByteString.Lazy.ByteString`s.
--
--   It uses `sendAll` internally to send all chunks sequentially. The lock on
--   the socket is acquired for each chunk separately, so the socket can be read
--   from in an interleaving fashion.
sendAllLazy :: Socket f Stream p -> LBS.ByteString -> MessageFlags -> IO ()
sendAllLazy s lbs flags =
  LBS.foldlChunks (\x bs-> x >> sendAll s bs flags) (return ()) lbs

-- | Sends a whole `BB.Builder` without allocating `BS.ByteString`s.
--   If performance is an issue, this operation should be preferred over all
--   other solutions for sending stream data.
--
--   The operation `alloca`tes a single buffer of the given size on entry and
--   reuses this buffer until the whole `BB.Builder` has been sent.
--   The count of all bytes sent is returned as there is no other efficient
--   way to determine a `BB.Builder`s size without actually building it.
sendAllBuilder :: Socket f Stream p -> Int -> BB.Builder -> MessageFlags -> IO Int64
sendAllBuilder s bufsize builder flags = do
  allocaBytes bufsize g
  where
    g ptr = writeStep (BB.runPut $ BB.putBuilder builder) 0
      where
        bufferRange :: BB.BufferRange
        bufferRange =
          BB.BufferRange ptr (plusPtr ptr bufsize)
        writeStep :: BB.BuildStep a -> Int64 -> IO Int64
        writeStep step alreadySent =
          BB.fillWithBuildStep step whenDone whenFull whenChunk bufferRange
          where
            whenDone ptrToNextFreeByte _
              | len > 0 = do
                  sendAllPtr ptr len
                  return $! alreadySent + fromIntegral len
              | otherwise =
                  return alreadySent
              where
                len = minusPtr ptrToNextFreeByte ptr
            whenFull ptrToNextFreeByte minBytesRequired nextStep
              | minBytesRequired > bufsize =
                  throwIO eNoBufferSpace
              | otherwise = do
                  sendAllPtr ptr len
                  writeStep nextStep $! alreadySent + fromIntegral len
              where
                len = minusPtr ptrToNextFreeByte ptr
            whenChunk ptrToNextFreeByte bs nextStep = do
              sendAllPtr ptr len
              if BS.null bs
                then
                  writeStep nextStep $! alreadySent + fromIntegral len
                else do
                  sendAll s bs flags
                  writeStep nextStep $! alreadySent + fromIntegral (len + BS.length bs)
              where
                len = minusPtr ptrToNextFreeByte ptr
    sendAllPtr :: Ptr Word8 -> Int -> IO ()
    sendAllPtr ptr len = do
      sent <- fromIntegral `fmap` unsafeSend s ptr (fromIntegral len) flags
      when (sent < len) $ sendAllPtr (plusPtr ptr sent) (len - sent)

-- | Like `receive`, but operates on lazy `Data.ByteString.Lazy.ByteString`s and
--   continues until either an empty part has been received (peer closed
--   the connection) or given buffer limit has been exceeded or an
--   exception occured.
--
--   - The `Data.Int.Int64` parameter is a soft limit on how many bytes to receive.
--     Collection is stopped if the limit has been exceeded. The result might
--     be up to one internal buffer size longer than the given limit.
--     If the returned `Data.ByteString.Lazy.ByteString`s length is lower or
--     eqal than the limit, the data has not been truncated and the
--     transmission is complete.
receiveAll :: Socket f Stream p -> Int64 -> MessageFlags -> IO LBS.ByteString
receiveAll sock maxLen flags = collect 0 Data.Monoid.mempty
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
                 $! (accum `Data.Monoid.mappend` BB.byteString bs)
    build accum = do
      return (BB.toLazyByteString accum)
