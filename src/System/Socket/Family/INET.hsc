{-# LANGUAGE TypeFamilies #-}
module System.Socket.Family.INET
  ( INET
  , AddrIn ()
  , SockAddrIn (..)
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
import System.Socket.Internal.FFI

#include "sys/types.h"
#include "sys/socket.h"
#include "sys/un.h"
#include "netinet/in.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data INET

instance Family INET where
  type Address INET = SockAddrIn
  familyNumber _ = (#const AF_INET)

data SockAddrIn
   = SockAddrIn
     { sinPort      :: Word16
     , sinAddr      :: AddrIn
     } deriving (Eq)

-- | To avoid errors with endianess it was decided to keep this type abstract.
--
--   Hint: Use the `Foreign.Storable.Storable` instance if you really need to access. It exposes it
--   exactly as found within an IP packet (big endian if you insist
--   on interpreting it as a number).
--
--   Another hint: Use `System.Socket.getAddrInfo` for parsing and suppress
--   nameserver lookups:
--
--   > > getAddrInfo (Just "127.0.0.1") Nothing aiNUMERICHOST :: IO [AddrInfo SockAddrIn STREAM TCP]
--   > [AddrInfo {addrInfoFlags = AddrInfoFlags 4, addrAddress = "127.0.0.1:0", addrCanonName = Nothing}]
newtype AddrIn
      = AddrIn BS.ByteString
      deriving (Eq)

-- | @0.0.0.0@
inaddrANY             :: AddrIn
inaddrANY              = AddrIn $ BS.pack [  0,  0,  0,  0]

-- | @255.255.255.0@
inaddrBROADCAST       :: AddrIn
inaddrBROADCAST        = AddrIn $ BS.pack [255,255,255,255]

-- | @255.255.255.0@
inaddrNONE            :: AddrIn
inaddrNONE             = AddrIn $ BS.pack [255,255,255,255]

-- | @127.0.0.1@
inaddrLOOPBACK        :: AddrIn
inaddrLOOPBACK         = AddrIn $ BS.pack [127,  0,  0,  1]

-- | @224.0.0.0@
inaddrUNSPEC_GROUP    :: AddrIn
inaddrUNSPEC_GROUP     = AddrIn $ BS.pack [224,  0,  0,  0]

-- | @224.0.0.1@
inaddrALLHOSTS_GROUP  :: AddrIn
inaddrALLHOSTS_GROUP   = AddrIn $ BS.pack [224,  0,  0,  1]

-- | @224.0.0.2@
inaddrALLRTS_GROUP    :: AddrIn
inaddrALLRTS_GROUP     = AddrIn $ BS.pack [224,  0,  0,  2]

-- | @224.0.0.255@
inaddrMAXLOCAL_GROUP  :: AddrIn
inaddrMAXLOCAL_GROUP   = AddrIn $ BS.pack [224,  0,  0,255]

instance Show SockAddrIn where
  show (SockAddrIn p a) =
    show a ++ ":" ++ show p

instance Show AddrIn where
  show (AddrIn a) =
    concat $ intersperse "." $ map show $ BS.unpack a

instance Storable AddrIn where
  sizeOf   _  = (#size      uint32_t)
  alignment _ = (#alignment uint32_t)
  peek ptr    =
    AddrIn <$> BS.packCStringLen (castPtr ptr, 4)
  poke ptr (AddrIn a) =
    BS.unsafeUseAsCString a $ \aPtr-> do
      copyBytes ptr (castPtr aPtr) (min 4 $ BS.length a)

instance Storable SockAddrIn where
  sizeOf    _ = (#size struct sockaddr_in)
  alignment _ = (#alignment struct sockaddr_in)
  peek ptr    = do
    ph  <- peekByteOff (sin_port ptr)  0 :: IO Word8
    pl  <- peekByteOff (sin_port ptr)  1 :: IO Word8
    a   <- peek        (sin_addr ptr)    :: IO AddrIn
    return (SockAddrIn (fromIntegral ph * 256 + fromIntegral pl) a)
    where
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
  poke ptr (SockAddrIn p a) = do
    c_memset ptr 0 (#const sizeof(struct sockaddr_in))
    poke        (sin_family   ptr) ((#const AF_INET) :: Word16)
    pokeByteOff (sin_port     ptr)  0 (fromIntegral $ rem (quot p 256) 256 :: Word8)
    pokeByteOff (sin_port     ptr)  1 (fromIntegral $ rem       p      256 :: Word8)
    poke        (sin_addr     ptr) a
    where
      sin_family   = (#ptr struct sockaddr_in, sin_family)
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)