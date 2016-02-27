{-# LANGUAGE TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module System.Socket.Family.Inet
  ( Inet
  , InetAddress
  , SocketAddress (SocketAddressInet, sinAddress, sinPort)
  -- * Special Addresses
  -- ** allHostsGroup
  , allHostsGroup
  -- ** any
  , System.Socket.Family.Inet.any
  -- ** broadcast
  , broadcast
  -- ** loopback
  , loopback
  -- ** maxLocalGroup
  , maxLocalGroup
  -- ** none
  , none
  -- ** unspecificGroup
  , unspecificGroup
  ) where

import Data.Word
import Data.List

import Foreign.Ptr
import Foreign.Storable

import System.Socket.Internal.Socket
import System.Socket.Internal.Platform

#include "hs_socket.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Inet

instance Family Inet where
  familyNumber _ = (#const AF_INET)

-- | Example:
--
--  > SocketAddressInet loopback 8080
data instance SocketAddress Inet
   = SocketAddressInet
     { sinAddress   :: InetAddress
     , sinPort      :: Word16
     } deriving (Eq, Show)

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
--   > [AddressInfo {addressInfoFlags = AddressInfoFlags 4, socketAddress = SocketAddressInet { sinAddress = 127.0.0.1, sinPort = 0}, canonicalName = Nothing}]
newtype InetAddress
      = InetAddress Word32
      deriving (Eq)

-- | @0.0.0.0@
any             :: InetAddress
any              = InetAddress $ 0

-- | @255.255.255.255@
broadcast       :: InetAddress
broadcast        = InetAddress $ foldl1' (\x y->x*256+y) [255,255,255,255]

-- | @255.255.255.255@
none            :: InetAddress
none             = InetAddress $ foldl1' (\x y->x*256+y) [255,255,255,255]

-- | @127.0.0.1@
loopback        :: InetAddress
loopback         = InetAddress $ foldl1' (\x y->x*256+y) [127,  0,  0,  1]

-- | @224.0.0.0@
unspecificGroup :: InetAddress
unspecificGroup  = InetAddress $ foldl1' (\x y->x*256+y) [224,  0,  0,  0]

-- | @224.0.0.1@
allHostsGroup   :: InetAddress
allHostsGroup    = InetAddress $ foldl1' (\x y->x*256+y) [224,  0,  0,  1]

-- | @224.0.0.255@
maxLocalGroup   :: InetAddress
maxLocalGroup    = InetAddress $ foldl1' (\x y->x*256+y) [224,  0,  0,255]

instance Show InetAddress where
  show (InetAddress a) =
   concat $ intersperse "."
          $ map (\p-> show $ a `div` 256^p `mod` 256) [3,2,1,0 :: Word32]

instance Storable InetAddress where
  sizeOf   _  = (#size      uint32_t)
  alignment _ = (#alignment uint32_t)
  peek ptr    = do
    i0  <- peekByteOff ptr 0 :: IO Word8
    i1  <- peekByteOff ptr 1 :: IO Word8
    i2  <- peekByteOff ptr 2 :: IO Word8
    i3  <- peekByteOff ptr 3 :: IO Word8
    return $ InetAddress $ (((((f i0 * 256) + f i1) * 256) + f i2) * 256) + f i3
    where
      f = fromIntegral
  poke ptr (InetAddress a) = do
    pokeByteOff ptr 0 (fromIntegral $ rem (quot a $ 256*256*256) 256 :: Word8)
    pokeByteOff ptr 1 (fromIntegral $ rem (quot a $     256*256) 256 :: Word8)
    pokeByteOff ptr 2 (fromIntegral $ rem (quot a $         256) 256 :: Word8)
    pokeByteOff ptr 3 (fromIntegral $ rem       a $              256 :: Word8)

instance Storable (SocketAddress Inet) where
  sizeOf    _ = (#size struct sockaddr_in)
  alignment _ = (#alignment struct sockaddr_in)
  peek ptr    = do
    ph  <- peekByteOff (sin_port ptr)  0 :: IO Word8
    pl  <- peekByteOff (sin_port ptr)  1 :: IO Word8
    a   <- peek        (sin_addr ptr)    :: IO InetAddress
    return (SocketAddressInet a (fromIntegral ph * 256 + fromIntegral pl))
    where
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
  poke ptr (SocketAddressInet a p) = do
    c_memset ptr 0 (#const sizeof(struct sockaddr_in))
    poke        (sin_family   ptr) ((#const AF_INET) :: Word16)
    pokeByteOff (sin_port     ptr)  0 (fromIntegral $ rem (quot p 256) 256 :: Word8)
    pokeByteOff (sin_port     ptr)  1 (fromIntegral $ rem       p      256 :: Word8)
    poke        (sin_addr     ptr) a
    where
      sin_family   = (#ptr struct sockaddr_in, sin_family)
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
