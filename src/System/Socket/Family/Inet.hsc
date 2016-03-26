{-# LANGUAGE TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Family.Inet
  ( -- * Inet
    Inet
    -- ** InetAddress
  , InetAddress
    -- ** InetPort
  , InetPort
    -- ** SocketAddress Inet
  , SocketAddress (SocketAddressInet, inetAddress, inetPort)
  -- * Special Addresses
  -- ** inetAllHostsGroup
  , inetAllHostsGroup
  -- ** inetAny
  , inetAny
  -- ** inetBroadcast
  , inetBroadcast
  -- ** inetLoopback
  , inetLoopback
  -- ** inetMaxLocalGroup
  , inetMaxLocalGroup
  -- ** inetNone
  , inetNone
  -- ** inetUnspecificGroup
  , inetUnspecificGroup
  ) where

import Data.Word
import Data.List

import Foreign.Ptr
import Foreign.Storable

import System.Socket.Internal.Socket
import System.Socket.Internal.Platform

#include "hs_socket.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | The [Internet Protocol version 4](https://en.wikipedia.org/wiki/IPv4).
data Inet

instance Family Inet where
  familyNumber _ = (#const AF_INET)

-- | An [IPv4](https://en.wikipedia.org/wiki/IPv4) socket address.
--
--   The socket address contains a port number that may be used by transport
--   protocols like [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol).
--
-- > SocketAddressInet inetLoopback 8080
data instance SocketAddress Inet
   = SocketAddressInet
     { inetAddress   :: InetAddress
     , inetPort      :: InetPort
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
--   > [AddressInfo {addressInfoFlags = AddressInfoFlags 4, socketAddress = SocketAddressInet {inetAddress = InetAddress 127.0.0.1, inetPort = InetPort 0}, canonicalName = Nothing}]
newtype InetAddress
      = InetAddress Word32
      deriving (Eq)

newtype InetPort = InetPort Word16
      deriving (Eq, Ord, Show, Num)

-- | @0.0.0.0@
inetAny             :: InetAddress
inetAny              = InetAddress $ 0

-- | @255.255.255.255@
inetBroadcast       :: InetAddress
inetBroadcast        = InetAddress $ foldl1' (\x y->x*256+y) [255,255,255,255]

-- | @255.255.255.255@
inetNone            :: InetAddress
inetNone             = InetAddress $ foldl1' (\x y->x*256+y) [255,255,255,255]

-- | @127.0.0.1@
inetLoopback        :: InetAddress
inetLoopback         = InetAddress $ foldl1' (\x y->x*256+y) [127,  0,  0,  1]

-- | @224.0.0.0@
inetUnspecificGroup :: InetAddress
inetUnspecificGroup  = InetAddress $ foldl1' (\x y->x*256+y) [224,  0,  0,  0]

-- | @224.0.0.1@
inetAllHostsGroup   :: InetAddress
inetAllHostsGroup    = InetAddress $ foldl1' (\x y->x*256+y) [224,  0,  0,  1]

-- | @224.0.0.255@
inetMaxLocalGroup   :: InetAddress
inetMaxLocalGroup    = InetAddress $ foldl1' (\x y->x*256+y) [224,  0,  0,255]

instance Show InetAddress where
  show (InetAddress a) = ("InetAddress " ++)
    $ concat
    $ intersperse "."
    $ map (\p-> show $ a `div` 256^p `mod` 256) [3,2,1,0 :: Word32]

instance Storable InetPort where
  sizeOf   _  = (#size      uint16_t)
  alignment _ = (#alignment uint16_t)
  peek ptr    = do
    p0 <- peekByteOff ptr 0 :: IO Word8
    p1 <- peekByteOff ptr 1 :: IO Word8
    return $ InetPort (fromIntegral p0 * 256 + fromIntegral p1)
  poke ptr (InetPort w16) = do
    pokeByteOff ptr 0 (w16_0 w16)
    pokeByteOff ptr 1 (w16_1 w16)
    where
      w16_0, w16_1 :: Word16 -> Word8
      w16_0 x = fromIntegral $ rem (quot x  256) 256
      w16_1 x = fromIntegral $ rem       x       256

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
    a  <- peek (sin_addr ptr)
    p  <- peek (sin_port ptr)
    return $ SocketAddressInet a p
    where
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
      sin_port     = (#ptr struct sockaddr_in, sin_port)
  poke ptr (SocketAddressInet a p) = do
    c_memset ptr 0 (#const sizeof(struct sockaddr_in))
    poke        (sin_family   ptr) ((#const AF_INET) :: Word16)
    poke        (sin_addr     ptr) a
    poke        (sin_port     ptr) p
    where
      sin_family   = (#ptr struct sockaddr_in, sin_family)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
      sin_port     = (#ptr struct sockaddr_in, sin_port)
