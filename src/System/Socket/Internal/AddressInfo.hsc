{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables,
            StandaloneDeriving, FlexibleContexts, TypeFamilies,
            GeneralizedNewtypeDeriving #-}
module System.Socket.Internal.AddressInfo (
    AddressInfo (..)
  , GetAddressInfo (..)
  , GetNameInfo (..)
  , AddressInfoException (..)
  , gaiStrerror
  , eaiAGAIN
  , eaiBADFLAGS
  , eaiFAIL
  , eaiFAMILY
  , eaiMEMORY
  , eaiNONAME
  , eaiSOCKTYPE
  , eaiSERVICE
  , eaiSYSTEM
  , AddressInfoFlags (..)
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

import System.IO.Unsafe

import System.Socket.Family
import System.Socket.Family.Inet
import System.Socket.Family.Inet6
import System.Socket.Type
import System.Socket.Protocol
import System.Socket.Internal.Platform

#include "hs_socket.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-------------------------------------------------------------------------------
-- AddressInfo
-------------------------------------------------------------------------------

data AddressInfo f t p
   = AddressInfo
     { flags         :: AddressInfoFlags
     , socketAddress :: SocketAddress f
     , canonicalName :: Maybe BS.ByteString
     }

deriving instance (Eq   (SocketAddress f)) => Eq   (AddressInfo f t p)
deriving instance (Show (SocketAddress f)) => Show (AddressInfo f t p)

-------------------------------------------------------------------------------
-- AddressInfoException
-------------------------------------------------------------------------------

-- | Contains the error code that can be matched against. Use `gaiStrerror`
--   to get a human readable explanation of the error (show`
--   does this as well).
newtype AddressInfoException
      = AddressInfoException CInt
   deriving (Eq, Typeable)

instance Show AddressInfoException where
  show e = "AddressInfoException \"" ++ gaiStrerror e ++ "\""

instance Exception AddressInfoException

-- | A wrapper around @gai_strerror@.
gaiStrerror :: AddressInfoException -> String
gaiStrerror (AddressInfoException e) =
  unsafePerformIO $ do
    msgPtr <- c_gai_strerror e
    peekCString msgPtr

-- | > AddressInfoException "Temporary failure in name resolution"
eaiAGAIN    :: AddressInfoException
eaiAGAIN     = AddressInfoException (#const EAI_AGAIN)

-- | > AddressInfoException "Bad value for ai_flags"
eaiBADFLAGS :: AddressInfoException
eaiBADFLAGS  = AddressInfoException (#const EAI_BADFLAGS)

-- | > AddressInfoException "Non-recoverable failure in name resolution"
eaiFAIL     :: AddressInfoException
eaiFAIL      = AddressInfoException (#const EAI_FAIL)

-- | > AddressInfoException "ai_family not supported"
eaiFAMILY   :: AddressInfoException
eaiFAMILY    = AddressInfoException (#const EAI_FAMILY)

-- | > AddressInfoException "Memory allocation failure"
eaiMEMORY   :: AddressInfoException
eaiMEMORY    = AddressInfoException (#const EAI_MEMORY)

-- | > AddressInfoException "No such host is known"
eaiNONAME   :: AddressInfoException
eaiNONAME    = AddressInfoException (#const EAI_NONAME)

-- | > AddressInfoException "Servname not supported for ai_socktype"
eaiSERVICE  :: AddressInfoException
eaiSERVICE   = AddressInfoException (#const EAI_SERVICE)

-- | > AddressInfoException "ai_socktype not supported"
eaiSOCKTYPE :: AddressInfoException
eaiSOCKTYPE  = AddressInfoException (#const EAI_SOCKTYPE)

-- | > AddressInfoException "System error"
eaiSYSTEM   :: AddressInfoException
eaiSYSTEM    = AddressInfoException (#const EAI_SYSTEM)


-- | Use the `Data.Monoid.Monoid` instance to combine several flags:
--
--   > mconcat [aiADDRCONFIG, aiV4MAPPED]
newtype AddressInfoFlags
      = AddressInfoFlags CInt
      deriving (Eq, Show, Bits)

instance Monoid AddressInfoFlags where
  mempty
    = AddressInfoFlags 0
  mappend (AddressInfoFlags a) (AddressInfoFlags b)
    = AddressInfoFlags (a .|. b)

aiADDRCONFIG  :: AddressInfoFlags
aiADDRCONFIG   = AddressInfoFlags (#const AI_ADDRCONFIG)

-- | Return both IPv4 (as mapped `SocketAddressInet6`) and IPv6 addresses when
-- `aiV4MAPPED` is set independent of whether IPv6 addresses exist for this
--  name.
aiALL         :: AddressInfoFlags
aiALL          = AddressInfoFlags (#const AI_ALL)

aiCANONNAME   :: AddressInfoFlags
aiCANONNAME    = AddressInfoFlags (#const AI_CANONNAME)

aiNUMERICHOST :: AddressInfoFlags
aiNUMERICHOST  = AddressInfoFlags (#const AI_NUMERICHOST)

aiNUMERICSERV :: AddressInfoFlags
aiNUMERICSERV  = AddressInfoFlags (#const AI_NUMERICSERV)

aiPASSIVE     :: AddressInfoFlags
aiPASSIVE      = AddressInfoFlags (#const AI_PASSIVE)

-- | Return mapped IPv4 addresses if no IPv6 addresses could be found
--   or if `aiALL` flag is set.
aiV4MAPPED    :: AddressInfoFlags
aiV4MAPPED     = AddressInfoFlags (#const AI_V4MAPPED)

-- | Use the `Data.Monoid.Monoid` instance to combine several flags:
--
--   > mconcat [niNAMEREQD, niNOFQDN]
newtype NameInfoFlags
      = NameInfoFlags CInt
      deriving (Eq, Show, Bits)

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

class (Family f) => GetAddressInfo f where
  -- | Maps names to addresses (i.e. by DNS lookup).
--
--   The operation throws `AddressInfoException`s.
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
--   > > getAddressInfo (Just "www.haskell.org") (Just "80") aiV4MAPPED :: IO [AddressInfo Inet6 STREAM TCP]
--   > [AddressInfo {addrInfoFlags = AddressInfoFlags 8, addrAddress = [2400:cb00:2048:0001:0000:0000:6ca2:cc3c]:80, addrCanonName = Nothing}]
--   > > getAddressInfo (Just "darcs.haskell.org") Nothing aiV4MAPPED :: IO [AddressInfo Inet6 STREAM TCP]
--   > [AddressInfo {addrInfoFlags = AddressInfoFlags 8, addrAddress = [0000:0000:0000:0000:0000:ffff:17fd:e1ad]:0, addrCanonName = Nothing}]
--   > > getAddressInfo (Just "darcs.haskell.org") Nothing mempty :: IO [AddressInfo Inet6 STREAM TCP]
--   > *** Exception: AddressInfoException "Name or service not known"
  getAddressInfo :: (Type t, Protocol p) => Maybe BS.ByteString -> Maybe BS.ByteString -> AddressInfoFlags -> IO [AddressInfo f t p]

instance GetAddressInfo Inet where
  getAddressInfo = getAddressInfo'

instance GetAddressInfo Inet6 where
  getAddressInfo = getAddressInfo'

getAddressInfo' :: forall f t p. (Family f, Type t, Protocol p) => Maybe BS.ByteString -> Maybe BS.ByteString -> AddressInfoFlags -> IO [AddressInfo f t p]
getAddressInfo' mnode mservice (AddressInfoFlags flags) = do
  alloca $ \resultPtrPtr-> do
    poke resultPtrPtr nullPtr
    allocaBytes (#size struct addrinfo) $ \addrInfoPtr-> do
      -- properly initialize the struct
      c_memset addrInfoPtr 0 (#const sizeof(struct addrinfo))
      poke (ai_flags addrInfoPtr) flags
      poke (ai_family addrInfoPtr) (familyNumber (undefined :: f))
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
                    peekAddressInfos resultPtr
                  else do
                    throwIO (AddressInfoException e)
            )
  where
    ai_flags     = (#ptr struct addrinfo, ai_flags)     :: Ptr (AddressInfo a t p) -> Ptr CInt
    ai_family    = (#ptr struct addrinfo, ai_family)    :: Ptr (AddressInfo a t p) -> Ptr CInt
    ai_socktype  = (#ptr struct addrinfo, ai_socktype)  :: Ptr (AddressInfo a t p) -> Ptr CInt
    ai_protocol  = (#ptr struct addrinfo, ai_protocol)  :: Ptr (AddressInfo a t p) -> Ptr CInt
    ai_addr      = (#ptr struct addrinfo, ai_addr)      :: Ptr (AddressInfo a t p) -> Ptr (Ptr a)
    ai_canonname = (#ptr struct addrinfo, ai_canonname) :: Ptr (AddressInfo a t p) -> Ptr CString
    ai_next      = (#ptr struct addrinfo, ai_next)      :: Ptr (AddressInfo a t p) -> Ptr (Ptr (AddressInfo a t p))
    fnode = case mnode of
      Just node    -> BS.useAsCString node
      Nothing      -> \f-> f nullPtr
    fservice = case mservice of
      Just service -> BS.useAsCString service
      Nothing      -> \f-> f nullPtr
    peekAddressInfos ptr = 
      if ptr == nullPtr
        then return []
        else do
          flag  <- peek (ai_flags ptr)
          addr  <- peek (ai_addr ptr) >>= peek
          cname <- do cnPtr <- peek (ai_canonname ptr)
                      if cnPtr == nullPtr
                        then return Nothing
                        else BS.packCString cnPtr >>= return . Just
          as    <- peek (ai_next ptr) >>= peekAddressInfos
          return ((AddressInfo (AddressInfoFlags flag) addr cname):as)

-- | Maps addresss to readable host- and service names.
--
--   The operation throws `AddressInfoException`s.
--
--   > > getNameInfo (SocketAddressInet 80 inaddrLOOPBACK) mempty
--   > ("localhost.localdomain","http")
class (Family f) => GetNameInfo f where
  getNameInfo :: SocketAddress f -> NameInfoFlags -> IO (BS.ByteString, BS.ByteString)

instance GetNameInfo Inet where
  getNameInfo = getNameInfo'

instance GetNameInfo Inet6 where
  getNameInfo = getNameInfo'

getNameInfo' :: Storable a => a -> NameInfoFlags -> IO (BS.ByteString, BS.ByteString)
getNameInfo' addr (NameInfoFlags flags) =
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
          throwIO (AddressInfoException e)