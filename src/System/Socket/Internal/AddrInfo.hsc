{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving, FlexibleContexts, TypeFamilies, CPP #-}
module System.Socket.Internal.AddrInfo (
    AddrInfo (..)
  , getAddrInfo
  , getAddrInfo6
  , getNameInfo
  , getNameInfo6
  , AddrInfoException (..)
  , aiStrError
  , eaiAGAIN
  , eaiBADFLAGS
  , eaiFAIL
  , eaiFAMILY
  , eaiMEMORY
  , eaiSOCKTYPE
  , eaiSERVICE
  , eaiSYSTEM
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

import System.IO.Unsafe

import System.Socket.Family
import System.Socket.Family.INET
import System.Socket.Family.INET6
import System.Socket.Type
import System.Socket.Protocol
import System.Socket.Internal.FFI

#include "hs_socket.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-------------------------------------------------------------------------------
-- AddrInfo
-------------------------------------------------------------------------------

data AddrInfo f t p
   = AddrInfo
     { addrInfoFlags :: AddrInfoFlags
     , addrAddress   :: Address f
     , addrCanonName :: Maybe BS.ByteString
     }

deriving instance (Eq   (Address f)) => Eq   (AddrInfo f t p)
deriving instance (Show (Address f)) => Show (AddrInfo f t p)

-------------------------------------------------------------------------------
-- AddrInfoException
-------------------------------------------------------------------------------

-- | Contains the error code that can be matched against. Use `aiStrError`
--   to get a human readable explanation of the error.
newtype AddrInfoException
      = AddrInfoException CInt
   deriving (Eq, Typeable)

instance Show AddrInfoException where
  show e = "AddrInfoException \"" ++ aiStrError e ++ "\""

instance Exception AddrInfoException

-- | A wrapper around @gai_strerror@.
aiStrError :: AddrInfoException -> String
aiStrError (AddrInfoException e) =
  unsafePerformIO $ do
    msgPtr <- c_gaistrerror e
    peekCString msgPtr

-- | > AddrInfoException "Temporary failure in name resolution"
eaiAGAIN    :: AddrInfoException
eaiAGAIN     = AddrInfoException (#const EAI_AGAIN)

-- | > AddrInfoException "Bad value for ai_flags"
eaiBADFLAGS :: AddrInfoException
eaiBADFLAGS  = AddrInfoException (#const EAI_BADFLAGS)

-- | > AddrInfoException "Non-recoverable failure in name resolution"
eaiFAIL     :: AddrInfoException
eaiFAIL      = AddrInfoException (#const EAI_FAIL)

-- | > AddrInfoException "ai_family not supported"
eaiFAMILY   :: AddrInfoException
eaiFAMILY    = AddrInfoException (#const EAI_FAMILY)

-- | > AddrInfoException "Memory allocation failure"
eaiMEMORY   :: AddrInfoException
eaiMEMORY    = AddrInfoException (#const EAI_MEMORY)

-- | > AddrInfoException "Servname not supported for ai_socktype"
eaiSERVICE  :: AddrInfoException
eaiSERVICE   = AddrInfoException (#const EAI_SERVICE)

-- | > AddrInfoException "ai_socktype not supported"
eaiSOCKTYPE :: AddrInfoException
eaiSOCKTYPE  = AddrInfoException (#const EAI_SOCKTYPE)

-- | > AddrInfoException "System error"
eaiSYSTEM   :: AddrInfoException
eaiSYSTEM    = AddrInfoException (#const EAI_SYSTEM)


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
--   > mconcat [niNAMEREQD, niNOFQDN]
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

getAddrInfo :: (Type t, Protocol p) => Maybe BS.ByteString -> Maybe BS.ByteString -> AddrInfoFlags -> IO [AddrInfo INET t p]
getAddrInfo = getAddrInfo'

-- | Like `getAddrInfo`, but for IPv6 addresses.
--
--   > > getAddrInfo (Just "www.haskell.org") (Just "80") aiV4MAPPED :: IO [AddrInfo INET6 STREAM TCP]
--   > [AddrInfo {addrInfoFlags = AddrInfoFlags 8, addrAddress = [2400:cb00:2048:0001:0000:0000:6ca2:cc3c]:80, addrCanonName = Nothing}]
--   > > getAddrInfo (Just "darcs.haskell.org") Nothing aiV4MAPPED :: IO [AddrInfo INET6 STREAM TCP]
--   > [AddrInfo {addrInfoFlags = AddrInfoFlags 8, addrAddress = [0000:0000:0000:0000:0000:ffff:17fd:e1ad]:0, addrCanonName = Nothing}]
--   > > getAddrInfo (Just "darcs.haskell.org") Nothing mempty :: IO [AddrInfo INET6 STREAM TCP]
--   > *** Exception: AddrInfoException "Name or service not known"
getAddrInfo6 :: (Type t, Protocol p) => Maybe BS.ByteString -> Maybe BS.ByteString -> AddrInfoFlags -> IO [AddrInfo INET6 t p]
getAddrInfo6 = getAddrInfo'

getAddrInfo' :: forall f t p. (Family f, Type t, Protocol p) => Maybe BS.ByteString -> Maybe BS.ByteString -> AddrInfoFlags -> IO [AddrInfo f t p]
getAddrInfo' mnode mservice (AddrInfoFlags flags) = do
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
                    peekAddrInfos resultPtr
                  else do
                    throwIO (AddrInfoException e)
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
--   > > getNameInfo (SockAddrIn 80 inaddrLOOPBACK) mempty
--   > ("localhost.localdomain","http")
getNameInfo :: SockAddrIn -> NameInfoFlags -> IO (BS.ByteString, BS.ByteString)
getNameInfo = getNameInfo'

-- | Like `getNameInfo`, but for IPv6 addresses.
getNameInfo6 :: SockAddrIn6 -> NameInfoFlags -> IO (BS.ByteString, BS.ByteString)
getNameInfo6 = getNameInfo'

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
          throwIO (AddrInfoException e)

-------------------------------------------------------------------------------
-- FFI
-------------------------------------------------------------------------------

foreign import ccall FFI_GETADDRINFO_SAFETY FFI_GETADDRINFO
  c_getaddrinfo  :: CString -> CString -> Ptr (AddrInfo a t p) -> Ptr (Ptr (AddrInfo a t p)) -> IO CInt

foreign import ccall FFI_FREEADDRINFO_SAFETY FFI_FREEADDRINFO
  c_freeaddrinfo :: Ptr (AddrInfo a t p) -> IO ()

foreign import ccall FFI_GETNAMEINFO_SAFETY FFI_GETNAMEINFO
  c_getnameinfo  :: Ptr a -> CInt -> CString -> CInt -> CString -> CInt -> CInt -> IO CInt

foreign import ccall FFI_GAISTRERROR_SAFETY FFI_GAISTRERROR
  c_gaistrerror  :: CInt -> IO CString
