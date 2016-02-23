{-# LANGUAGE TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module System.Socket.Family.Inet6
  ( Inet6
  , Inet6Address
  , SocketAddress (SocketAddressInet6, address, port, flowInfo, scopeId)
  -- * Special Addresses
  -- ** any
  , System.Socket.Family.Inet6.any
  -- ** loopback
  , loopback
  -- * Socket Options
  -- ** V6Only
  , V6Only (..)
  ) where

import Data.Bits
import Data.Monoid
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Control.Applicative

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Utils

import System.Socket.Internal.Socket
import System.Socket.Internal.Platform

#include "hs_socket.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Inet6

instance Family Inet6 where
  familyNumber _ = (#const AF_INET6)

-- | Example:
--
--  > SocketAddressInet6 loopback 8080 mempty 0
data instance SocketAddress Inet6
   = SocketAddressInet6
     { address   :: Inet6Address
     , port      :: Word16
     , flowInfo  :: Word32
     , scopeId   :: Word32
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
--   > > getAddressInfo (Just "::1") Nothing aiNumericHost :: IO [AddressInfo SocketAddressInet6 Stream TCP]
--   > [AddressInfo {
--   >    addressInfoFlags = AddressInfoFlags 4,
--   >    socketAddress    = SocketAddressInet6 {address = 0000:0000:0000:0000:0000:0000:0000:0001, port = 0, flowInfo = mempty, scopeId = 0},
--   >    canonicalName    = Nothing }]
newtype Inet6Address
      = Inet6Address BS.ByteString
      deriving (Eq)

-- | @::@
any      :: Inet6Address
any       = Inet6Address (BS.pack [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0])

-- | @::1@
loopback :: Inet6Address
loopback  = Inet6Address (BS.pack [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1])

instance Show Inet6Address where
  show (Inet6Address addr) = tail $ t $ BS.unpack addr
    where
      t []       = []
      t [x]      = g x 0 []
      t (x:y:xs) = g x y (t xs)
      g x y s    = let (a,b) = quotRem x 16
                       (c,d) = quotRem y 16
                   in  ':':(h a):(h b):(h c):(h d):s
      h :: Word8 -> Char
      h 0  = '0'
      h 1  = '1'
      h 2  = '2'
      h 3  = '3'
      h 4  = '4'
      h 5  = '5'
      h 6  = '6'
      h 7  = '7'
      h 8  = '8'
      h 9  = '9'
      h 10 = 'a'
      h 11 = 'b'
      h 12 = 'c'
      h 13 = 'd'
      h 14 = 'e'
      h 15 = 'f'
      h  _ = '_'

instance Storable Inet6Address where
  sizeOf   _  = 16
  alignment _ = 16
  peek ptr    =
    Inet6Address <$> BS.packCStringLen (castPtr ptr, 16)
  poke ptr (Inet6Address a) =
    BS.unsafeUseAsCString a $ \aPtr-> do
      copyBytes ptr (castPtr aPtr) (min 16 $ BS.length a)

instance Storable (SocketAddress Inet6) where
  sizeOf    _ = (#size struct sockaddr_in6)
  alignment _ = (#alignment struct sockaddr_in6)
  peek ptr    = do
    f   <- peek              (sin6_flowinfo ptr)     :: IO Word32
    ph  <- peekByteOff       (sin6_port     ptr)  0  :: IO Word8
    pl  <- peekByteOff       (sin6_port     ptr)  1  :: IO Word8
    a   <- peek              (sin6_addr     ptr)     :: IO Inet6Address
    s   <- peek              (sin6_scope_id ptr)     :: IO Word32
    return (SocketAddressInet6 a (fromIntegral ph * 256 + fromIntegral pl) f s)
    where
      sin6_flowinfo = (#ptr struct sockaddr_in6, sin6_flowinfo)
      sin6_scope_id = (#ptr struct sockaddr_in6, sin6_scope_id)
      sin6_port     = (#ptr struct sockaddr_in6, sin6_port)
      sin6_addr     = (#ptr struct in6_addr, s6_addr) . (#ptr struct sockaddr_in6, sin6_addr)
  poke ptr (SocketAddressInet6 a p f s) = do
    c_memset ptr 0 (#const sizeof(struct sockaddr_in6))
    poke        (sin6_family   ptr) ((#const AF_INET6) :: Word16)
    poke        (sin6_flowinfo ptr) f
    poke        (sin6_scope_id ptr) s
    pokeByteOff (sin6_port     ptr)  0 (fromIntegral $ rem (quot p 256) 256 :: Word8)
    pokeByteOff (sin6_port     ptr)  1 (fromIntegral $ rem       p      256 :: Word8)
    poke        (sin6_addr     ptr) a
    where
      sin6_family   = (#ptr struct sockaddr_in6, sin6_family)
      sin6_flowinfo = (#ptr struct sockaddr_in6, sin6_flowinfo)
      sin6_scope_id = (#ptr struct sockaddr_in6, sin6_scope_id)
      sin6_port     = (#ptr struct sockaddr_in6, sin6_port)
      sin6_addr     = (#ptr struct in6_addr, s6_addr) . (#ptr struct sockaddr_in6, sin6_addr)

-------------------------------------------------------------------------------
-- Address family specific socket options
-------------------------------------------------------------------------------

-- | @IPV6_V6ONLY@
data V6Only
   = V6Only Bool
   deriving (Eq, Ord, Show)

instance GetSocketOption V6Only where
  getSocketOption s =
    V6Only . ((/=0) :: CInt -> Bool) <$> unsafeGetSocketOption s (#const IPPROTO_IPV6) (#const IPV6_V6ONLY)

instance SetSocketOption V6Only where
  setSocketOption s (V6Only o) =
    unsafeSetSocketOption s (#const IPPROTO_IPV6) (#const IPV6_V6ONLY) (if o then 1 else 0 :: CInt)
