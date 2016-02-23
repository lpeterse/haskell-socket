{-# LANGUAGE TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module System.Socket.Family.Inet
  ( Inet
  -- * Addresses
  , Address (InetAddress, address, port)
  , IPv4Address ()
  , Port (..)
  -- ** Special Address Constants
  -- *** allHostsGroup
  , allHostsGroup
  -- *** any
  , System.Socket.Family.Inet.any
  -- *** broadcast
  , broadcast
  -- *** loopback
  , loopback
  -- *** maxLocalGroup
  , maxLocalGroup
  -- *** none
  , none
  -- *** unspecificGroup
  , unspecificGroup
  -- * Socket Options
  ) where

import Data.Word
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Control.Applicative

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils

import System.Socket.Family
import System.Socket.Internal.Platform

#include "hs_socket.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Inet

instance Family Inet where
  familyNumber _ = (#const AF_INET)

data instance Address Inet
   = InetAddress
     { address   :: IPv4Address
     , port      :: Port
     } deriving (Eq, Show)

newtype Port
      = Port Word16
      deriving (Eq, Ord, Num)

instance Show Port where
  show (Port p) = show p

-- | To avoid errors with endianess it was decided to keep this type abstract.
--
--   Hint: Use the `Foreign.Storable.Storable` instance if you really need to access. It exposes it
--   exactly as found within an IP packet (big endian if you insist
--   on interpreting it as a number).
--
--   Another hint: Use `System.Socket.getAddressInfo` for parsing and suppress
--   nameserver lookups:
--
--   > > getAddressInfo (Just "127.0.0.1") Nothing aiNumericHost :: IO [AddressInfo Inet Stream TCP]
--   > [AddressInfo {addressInfoFlags = AddressInfoFlags 4, socketAddress = SocketAddressInet { address = 127.0.0.1, port = 0}, canonicalName = Nothing}]
newtype IPv4Address
      = IPv4Address BS.ByteString
      deriving (Eq)

-- | @0.0.0.0@
any             :: IPv4Address
any              = IPv4Address $ BS.pack [  0,  0,  0,  0]

-- | @255.255.255.255@
broadcast       :: IPv4Address
broadcast        = IPv4Address $ BS.pack [255,255,255,255]

-- | @255.255.255.255@
none            :: IPv4Address
none             = IPv4Address $ BS.pack [255,255,255,255]

-- | @127.0.0.1@
loopback        :: IPv4Address
loopback         = IPv4Address $ BS.pack [127,  0,  0,  1]

-- | @224.0.0.0@
unspecificGroup :: IPv4Address
unspecificGroup  = IPv4Address $ BS.pack [224,  0,  0,  0]

-- | @224.0.0.1@
allHostsGroup   :: IPv4Address
allHostsGroup    = IPv4Address $ BS.pack [224,  0,  0,  1]

-- | @224.0.0.255@
maxLocalGroup   :: IPv4Address
maxLocalGroup    = IPv4Address $ BS.pack [224,  0,  0,255]

instance Show IPv4Address where
  show (IPv4Address a) =
    concat $ intersperse "." $ map show $ BS.unpack a

instance Storable IPv4Address where
  sizeOf   _  = (#size      uint32_t)
  alignment _ = (#alignment uint32_t)
  peek ptr    =
    IPv4Address <$> BS.packCStringLen (castPtr ptr, 4)
  poke ptr (IPv4Address a) =
    BS.unsafeUseAsCString a $ \aPtr-> do
      copyBytes ptr (castPtr aPtr) (min 4 $ BS.length a)

instance Storable (Address Inet) where
  sizeOf    _ = (#size struct sockaddr_in)
  alignment _ = (#alignment struct sockaddr_in)
  peek ptr    = do
    ph  <- peekByteOff (sin_port ptr)  0 :: IO Word8
    pl  <- peekByteOff (sin_port ptr)  1 :: IO Word8
    a   <- peek        (sin_addr ptr)    :: IO IPv4Address
    return (InetAddress a (Port $ fromIntegral ph * 256 + fromIntegral pl))
    where
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
  poke ptr (InetAddress a (Port p)) = do
    c_memset ptr 0 (#const sizeof(struct sockaddr_in))
    poke        (sin_family   ptr) ((#const AF_INET) :: Word16)
    pokeByteOff (sin_port     ptr)  0 (fromIntegral $ rem (quot p 256) 256 :: Word8)
    pokeByteOff (sin_port     ptr)  1 (fromIntegral $ rem       p      256 :: Word8)
    poke        (sin_addr     ptr) a
    where
      sin_family   = (#ptr struct sockaddr_in, sin_family)
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
