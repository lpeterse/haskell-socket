module System.Socket.Type.Stream (
  -- * Stream
    Stream
  -- * Convenience Operations
  -- ** sendAll, receiveAll
  , sendAll
  , receiveAll
  ) where

import Control.Monad (when)
import Data.Int
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.Lazy as LBS

import System.Socket
import System.Socket.Internal.Socket
import System.Socket.Internal.Message

#include "hs_socket.h"

data Stream

instance Type Stream where
  typeNumber _ = (#const SOCK_STREAM)

-------------------------------------------------------------------------------
-- Convenience Operations
-------------------------------------------------------------------------------

-- | Like `send`, but operates on lazy `Data.ByteString.Lazy.ByteString`s and
--   continues until all data has been sent or an exception occured.
sendAll ::Socket f Stream p -> LBS.ByteString -> MessageFlags -> IO ()
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
