{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module System.Socket.Internal.AddrInfo (
    AddrInfo (..)
  , AddrInfoException (..)
  , getAddrInfo
  , getNameInfo
  , AddrInfoFlags (..)
  , aiADDRCONFIG
  , aiALL
  , aiCANONNAME
  , aiNUMERICHOST
  , aiNUMERICSERV
  , aiPASSIVE
  , aiV4MAPPED
  , NameInfoFlags (..)
  , niNAMEREQD
  , niDGRAM
  , niNOFQDN
  , niNUMERICHOST
  , niNUMERICSERV
  ) where

import Control.Exception
import Control.Monad

import Data.Bits
import Data.Monoid
import Data.Typeable
import qualified Data.ByteString as BS

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc

import System.Socket.Address
import System.Socket.Type
import System.Socket.Protocol

#include "sys/types.h"
#include "sys/socket.h"
#include "netdb.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-------------------------------------------------------------------------------
-- AddrInfo
-------------------------------------------------------------------------------

data AddrInfo a t p
   = AddrInfo
     { addrInfoFlags :: AddrInfoFlags
     , addrAddress   :: a
     , addrCanonName :: Maybe BS.ByteString
     } deriving (Eq, Show)

-------------------------------------------------------------------------------
-- AddrInfoException
-------------------------------------------------------------------------------

-- | Contains the error code that can be matched against and a readable
--   description taken from @eia_strerr@.
data AddrInfoException
   = AddrInfoException CInt String
   deriving (Eq, Show, Typeable)

instance Exception AddrInfoException

-- | Use the `Data.Monoid.Monoid` instance to combine several flags:
--
--   > mconcat [aiADDRCONFIG, aiV4MAPPED]
newtype AddrInfoFlags
      = AddrInfoFlags CInt
      deriving (Eq, Show)

instance Monoid AddrInfoFlags where
  mempty
    = AddrInfoFlags 0
  mappend (AddrInfoFlags a) (AddrInfoFlags b)
    = AddrInfoFlags (a .|. b)

aiADDRCONFIG  :: AddrInfoFlags
aiADDRCONFIG   = AddrInfoFlags (#const AI_ADDRCONFIG)

aiALL         :: AddrInfoFlags
aiALL          = AddrInfoFlags (#const AI_ALL)

aiCANONNAME   :: AddrInfoFlags
aiCANONNAME    = AddrInfoFlags (#const AI_CANONNAME)

aiNUMERICHOST :: AddrInfoFlags
aiNUMERICHOST  = AddrInfoFlags (#const AI_NUMERICHOST)

aiNUMERICSERV :: AddrInfoFlags
aiNUMERICSERV  = AddrInfoFlags (#const AI_NUMERICSERV)

aiPASSIVE     :: AddrInfoFlags
aiPASSIVE      = AddrInfoFlags (#const AI_PASSIVE)

aiV4MAPPED    :: AddrInfoFlags
aiV4MAPPED     = AddrInfoFlags (#const AI_V4MAPPED)

-- | Use the `Data.Monoid.Monoid` instance to combine several flags:
--
--   > mconcat [aiADDRCONFIG, aiV4MAPPED]
newtype NameInfoFlags
      = NameInfoFlags CInt
      deriving (Eq, Show)

instance Monoid NameInfoFlags where
  mempty
    = NameInfoFlags 0
  mappend (NameInfoFlags a) (NameInfoFlags b)
    = NameInfoFlags (a .|. b)

-- | Throw an exception if the hostname cannot be determined.
niNAMEREQD     :: NameInfoFlags
niNAMEREQD      = NameInfoFlags (#const NI_NAMEREQD)

-- | Service is datagram based (UDP) rather than stream based (TCP).
niDGRAM        :: NameInfoFlags
niDGRAM         = NameInfoFlags (#const NI_DGRAM)

-- | Return only the hostname part of the fully qualified domain name for local hosts.
niNOFQDN       :: NameInfoFlags
niNOFQDN        = NameInfoFlags (#const NI_NOFQDN)

-- | Return the numeric form of the host address.
niNUMERICHOST  :: NameInfoFlags
niNUMERICHOST   = NameInfoFlags (#const NI_NUMERICHOST)

-- | Return the numeric form of the service address.
niNUMERICSERV  :: NameInfoFlags
niNUMERICSERV   = NameInfoFlags (#const NI_NUMERICSERV)

-- | Maps names to addresses (i.e. by DNS lookup).
--
--   The operation throws `AddrInfoException`s.
--
--   Contrary to the underlying @getaddrinfo@ operation this wrapper is
--   typesafe and thus only returns records that match the address, type
--   and protocol encoded in the type. This is the price we have to pay
--   for typesafe sockets and extensibility.
--
--   If you need different types of records, you need to start several
--   queries. If you want to connect to both IPv4 and IPV6 addresses use
--   `aiV4MAPPED` and use IPv6-sockets.
--
--   > > getAddrInfo (Just "www.haskell.org") (Just "80") aiV4MAPPED :: IO [AddrInfo SockAddrIn6 STREAM TCP]
--   > [AddrInfo {addrInfoFlags = AddrInfoFlags 8, addrAddress = "[2400:cb00:2048:0001:0000:0000:6ca2:cc3c]:80", addrCanonName = Nothing}]
--   > > getAddrInfo (Just "darcs.haskell.org") Nothing aiV4MAPPED :: IO [AddrInfo SockAddrIn6 STREAM TCP]
--   > [AddrInfo {addrInfoFlags = AddrInfoFlags 8, addrAddress = "[0000:0000:0000:0000:0000:ffff:17fd:e1ad]:0", addrCanonName = Nothing}]
--   > > getAddrInfo (Just "darcs.haskell.org") Nothing mempty :: IO [AddrInfo SockAddrIn6 STREAM TCP]
--   > *** Exception: AddrInfoException (-2) "Name or service not known"
getAddrInfo :: (Address a, Type t, Protocol p) => Maybe BS.ByteString -> Maybe BS.ByteString -> AddrInfoFlags -> IO [AddrInfo a t p]
getAddrInfo = getAddrInfo'
  where
    getAddrInfo' :: forall a t p. (Address a, Type t, Protocol p) => Maybe BS.ByteString -> Maybe BS.ByteString -> AddrInfoFlags -> IO [AddrInfo a t p]
    getAddrInfo' mnode mservice (AddrInfoFlags flags) = do
      alloca $ \resultPtrPtr-> do
        poke resultPtrPtr nullPtr
        allocaBytes (#size struct addrinfo) $ \addrInfoPtr-> do
          -- properly initialize the struct
          c_memset addrInfoPtr 0 (#const sizeof(struct addrinfo))
          poke (ai_flags addrInfoPtr) flags
          poke (ai_family addrInfoPtr) (addressFamilyNumber (undefined :: a))
          poke (ai_socktype addrInfoPtr) (typeNumber (undefined :: t))
          poke (ai_protocol addrInfoPtr) (protocolNumber (undefined :: p))
          fnode $ \nodePtr-> do
            fservice $ \servicePtr->
              bracket
                (c_getaddrinfo nodePtr servicePtr addrInfoPtr resultPtrPtr)
                (\_-> do resultPtr <- peek resultPtrPtr
                         when (resultPtr /= nullPtr) (c_freeaddrinfo resultPtr)
                )
                (\e-> if e == 0 then do
                        resultPtr <- peek resultPtrPtr
                        peekAddrInfos resultPtr
                      else do
                        msgPtr <- c_gaistrerror e
                        msg <- peekCString msgPtr
                        throwIO (AddrInfoException e msg)
                )
      where
        ai_flags     = (#ptr struct addrinfo, ai_flags)     :: Ptr (AddrInfo a t p) -> Ptr CInt
        ai_family    = (#ptr struct addrinfo, ai_family)    :: Ptr (AddrInfo a t p) -> Ptr CInt
        ai_socktype  = (#ptr struct addrinfo, ai_socktype)  :: Ptr (AddrInfo a t p) -> Ptr CInt
        ai_protocol  = (#ptr struct addrinfo, ai_protocol)  :: Ptr (AddrInfo a t p) -> Ptr CInt
        ai_addr      = (#ptr struct addrinfo, ai_addr)      :: Ptr (AddrInfo a t p) -> Ptr (Ptr a)
        ai_canonname = (#ptr struct addrinfo, ai_canonname) :: Ptr (AddrInfo a t p) -> Ptr CString
        ai_next      = (#ptr struct addrinfo, ai_next)      :: Ptr (AddrInfo a t p) -> Ptr (Ptr (AddrInfo a t p))
        fnode = case mnode of
          Just node    -> BS.useAsCString node
          Nothing      -> \f-> f nullPtr
        fservice = case mservice of
          Just service -> BS.useAsCString service
          Nothing      -> \f-> f nullPtr
        peekAddrInfos ptr = 
          if ptr == nullPtr
            then return []
            else do
              flag  <- peek (ai_flags ptr)
              addr  <- peek (ai_addr ptr) >>= peek
              cname <- do cnPtr <- peek (ai_canonname ptr)
                          if cnPtr == nullPtr
                            then return Nothing
                            else BS.packCString cnPtr >>= return . Just
              as    <- peek (ai_next ptr) >>= peekAddrInfos
              return ((AddrInfo (AddrInfoFlags flag) addr cname):as)

-- | Maps addresss to readable host- and service names.
--
--   The operation throws `AddrInfoException`s.
--
--   > > getNameInfo (SockAddrIn 80 $ pack [23,253,242,70]) mempty
--   > ("haskell.org","http")
getNameInfo :: (Address a) => a -> NameInfoFlags -> IO (BS.ByteString, BS.ByteString)
getNameInfo addr (NameInfoFlags flags) =
  alloca $ \addrPtr->
    allocaBytes (#const NI_MAXHOST) $ \hostPtr->
      allocaBytes (#const NI_MAXSERV) $ \servPtr-> do
        poke addrPtr addr
        e <- c_getnameinfo addrPtr (fromIntegral $ sizeOf addr)
                           hostPtr (#const NI_MAXHOST)
                           servPtr (#const NI_MAXSERV)
                           flags
        if e == 0 then do
          host <- BS.packCString hostPtr
          serv <- BS.packCString servPtr
          return (host,serv)
        else do
          msgPtr <- c_gaistrerror e
          msg <- peekCString msgPtr
          throwIO (AddrInfoException e msg)

-------------------------------------------------------------------------------
-- FFI
-------------------------------------------------------------------------------

foreign import ccall safe "netdb.h getaddrinfo"
  c_getaddrinfo  :: CString -> CString -> Ptr (AddrInfo a t p) -> Ptr (Ptr (AddrInfo a t p)) -> IO CInt

foreign import ccall unsafe "netdb.h freeaddrinfo"
  c_freeaddrinfo :: Ptr (AddrInfo a t p) -> IO ()

foreign import ccall safe "netdb.h getnameinfo"
  c_getnameinfo  :: Ptr a -> CInt -> CString -> CInt -> CString -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "netdb.h gai_strerror"
  c_gaistrerror  :: CInt -> IO CString

foreign import ccall unsafe "string.h memset"
  c_memset       :: Ptr a -> CInt -> CSize -> IO ()

