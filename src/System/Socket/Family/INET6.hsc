{-# LANGUAGE TypeFamilies #-}
module System.Socket.Family.INET6
  ( INET6
  , AddrIn6 ()
  , SockAddrIn6 (..)
  , in6addrANY
  , in6addrLOOPBACK
  ) where

import Data.Word
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

data INET6

instance Family INET6 where
  type Address INET6 = SockAddrIn6
  familyNumber _ = (#const AF_INET6)

data SockAddrIn6
   = SockAddrIn6
     { sin6Port      :: Word16
     , sin6Flowinfo  :: Word32
     , sin6Addr      :: AddrIn6
     , sin6ScopeId   :: Word32
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
--   > > getAddrInfo (Just "::1") Nothing aiNUMERICHOST :: IO [AddrInfo SockAddrIn6 STREAM TCP]
--   > [AddrInfo {addrInfoFlags = AddrInfoFlags 4, addrAddress = [0000:0000:0000:0000:0000:0000:0000:0001]:0, addrCanonName = Nothing}]
newtype AddrIn6
      = AddrIn6 BS.ByteString
      deriving (Eq)

-- | @::@
in6addrANY      :: AddrIn6
in6addrANY       = AddrIn6 (BS.pack [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0])

-- | @::1@
in6addrLOOPBACK :: AddrIn6
in6addrLOOPBACK  = AddrIn6 (BS.pack [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1])

instance Show SockAddrIn6 where
  show (SockAddrIn6 p _ addr _) =
    "[" ++ show addr ++ "]:" ++ show p

instance Show AddrIn6 where
  show (AddrIn6 addr) = tail $ t $ BS.unpack addr
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

instance Storable AddrIn6 where
  sizeOf   _  = 16
  alignment _ = 16
  peek ptr    =
    AddrIn6 <$> BS.packCStringLen (castPtr ptr, 16)
  poke ptr (AddrIn6 a) =
    BS.unsafeUseAsCString a $ \aPtr-> do
      copyBytes ptr (castPtr aPtr) (min 16 $ BS.length a)

instance Storable SockAddrIn6 where
  sizeOf    _ = (#size struct sockaddr_in6)
  alignment _ = (#alignment struct sockaddr_in6)
  peek ptr    = do
    f   <- peek              (sin6_flowinfo ptr)     :: IO Word32
    ph  <- peekByteOff       (sin6_port     ptr)  0  :: IO Word8
    pl  <- peekByteOff       (sin6_port     ptr)  1  :: IO Word8
    a   <- peek              (sin6_addr     ptr)     :: IO AddrIn6
    s   <- peek              (sin6_scope_id ptr)     :: IO Word32
    return (SockAddrIn6 (fromIntegral ph * 256 + fromIntegral pl) f a s)
    where
      sin6_flowinfo = (#ptr struct sockaddr_in6, sin6_flowinfo)
      sin6_scope_id = (#ptr struct sockaddr_in6, sin6_scope_id)
      sin6_port     = (#ptr struct sockaddr_in6, sin6_port)
      sin6_addr     = (#ptr struct in6_addr, s6_addr) . (#ptr struct sockaddr_in6, sin6_addr)
  poke ptr (SockAddrIn6 p f a s) = do
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