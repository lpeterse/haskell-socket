module System.Socket.Address.Unix
  ( Unix (..)
  ) where

import Data.Word
import qualified Data.ByteString as BS

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils

import System.Socket.Address

#include "sys/types.h"
#include "sys/socket.h"
#include "sys/un.h"
#include "netinet/in.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Address Unix where
  addressFamilyNumber _ = (#const AF_UNIX)

data Unix
   = SockAddrUn
     { sunPath :: BS.ByteString
     } deriving (Eq, Ord, Show)

instance Storable Unix where
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
      truncatedPath = BS.take ( sizeOf (undefined :: Unix)
                              - sizeOf (undefined :: Word16)
                              - 1 ) path