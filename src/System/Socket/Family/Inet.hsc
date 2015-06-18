{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module System.Socket.Family.Inet
  ( Inet
  , SocketAddressInet (..)
  , InetPort (..)
  , InetAddress ()
  , inaddrANY
  , inaddrBROADCAST
  , inaddrNONE
  , inaddrLOOPBACK
  , inaddrUNSPEC_GROUP
  , inaddrALLHOSTS_GROUP
  , inaddrALLRTS_GROUP
  , inaddrMAXLOCAL_GROUP
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
  type SocketAddress Inet = SocketAddressInet
  familyNumber _ = (#const AF_INET)

data SocketAddressInet
   = SocketAddressInet
     { port      :: InetPort
     , address   :: InetAddress
     } deriving (Eq)

newtype InetPort
      = InetPort Word16
      deriving (Eq, Ord, Show, Num)

-- | To avoid errors with endianess it was decided to keep this type abstract.
--
--   Hint: Use the `Foreign.Storable.Storable` instance if you really need to access. It exposes it
--   exactly as found within an IP packet (big endian if you insist
--   on interpreting it as a number).
--
--   Another hint: Use `System.Socket.getAddressInfo` for parsing and suppress
--   nameserver lookups:
--
--   > > getAddressInfo (Just "127.0.0.1") Nothing aiNUMERICHOST :: IO [AddressInfo SocketAddressInet STREAM TCP]
--   > [AddressInfo {addrInfoFlags = AddressInfoFlags 4, addrAddress = "127.0.0.1:0", addrCanonName = Nothing}]
newtype InetAddress
      = InetAddress BS.ByteString
      deriving (Eq)

-- | @0.0.0.0@
inaddrANY             :: InetAddress
inaddrANY              = InetAddress $ BS.pack [  0,  0,  0,  0]

-- | @255.255.255.255@
inaddrBROADCAST       :: InetAddress
inaddrBROADCAST        = InetAddress $ BS.pack [255,255,255,255]

-- | @255.255.255.255@
inaddrNONE            :: InetAddress
inaddrNONE             = InetAddress $ BS.pack [255,255,255,255]

-- | @127.0.0.1@
inaddrLOOPBACK        :: InetAddress
inaddrLOOPBACK         = InetAddress $ BS.pack [127,  0,  0,  1]

-- | @224.0.0.0@
inaddrUNSPEC_GROUP    :: InetAddress
inaddrUNSPEC_GROUP     = InetAddress $ BS.pack [224,  0,  0,  0]

-- | @224.0.0.1@
inaddrALLHOSTS_GROUP  :: InetAddress
inaddrALLHOSTS_GROUP   = InetAddress $ BS.pack [224,  0,  0,  1]

-- | @224.0.0.2@
inaddrALLRTS_GROUP    :: InetAddress
inaddrALLRTS_GROUP     = InetAddress $ BS.pack [224,  0,  0,  2]

-- | @224.0.0.255@
inaddrMAXLOCAL_GROUP  :: InetAddress
inaddrMAXLOCAL_GROUP   = InetAddress $ BS.pack [224,  0,  0,255]

instance Show SocketAddressInet where
  show (SocketAddressInet p a) =
    show a ++ ":" ++ show p

instance Show InetAddress where
  show (InetAddress a) =
    concat $ intersperse "." $ map show $ BS.unpack a

instance Storable InetAddress where
  sizeOf   _  = (#size      uint32_t)
  alignment _ = (#alignment uint32_t)
  peek ptr    =
    InetAddress <$> BS.packCStringLen (castPtr ptr, 4)
  poke ptr (InetAddress a) =
    BS.unsafeUseAsCString a $ \aPtr-> do
      copyBytes ptr (castPtr aPtr) (min 4 $ BS.length a)

instance Storable SocketAddressInet where
  sizeOf    _ = (#size struct sockaddr_in)
  alignment _ = (#alignment struct sockaddr_in)
  peek ptr    = do
    ph  <- peekByteOff (sin_port ptr)  0 :: IO Word8
    pl  <- peekByteOff (sin_port ptr)  1 :: IO Word8
    a   <- peek        (sin_addr ptr)    :: IO InetAddress
    return (SocketAddressInet (InetPort $ fromIntegral ph * 256 + fromIntegral pl) a)
    where
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
  poke ptr (SocketAddressInet (InetPort p) a) = do
    c_memset ptr 0 (#const sizeof(struct sockaddr_in))
    poke        (sin_family   ptr) ((#const AF_INET) :: Word16)
    pokeByteOff (sin_port     ptr)  0 (fromIntegral $ rem (quot p 256) 256 :: Word8)
    pokeByteOff (sin_port     ptr)  1 (fromIntegral $ rem       p      256 :: Word8)
    poke        (sin_addr     ptr) a
    where
      sin_family   = (#ptr struct sockaddr_in, sin_family)
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)