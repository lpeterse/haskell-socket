{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving,
             FlexibleContexts, TypeFamilies, GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Internal.AddressInfo (
    AddressInfo (..)
  , HasAddressInfo (..)
  , NameInfo (..)
  , HasNameInfo (..)
  , AddressInfoException (..)
  , eaiAgain
  , eaiBadFlags
  , eaiFail
  , eaiFamily
  , eaiMemory
  , eaiNoName
  , eaiSocketType
  , eaiService
  , eaiSystem
  , AddressInfoFlags (..)
  , aiAddressConfig
  , aiAll
  , aiCanonicalName
  , aiNumericHost
  , aiNumericService
  , aiPassive
  , aiV4Mapped
  , NameInfoFlags (..)
  , niNameRequired
  , niDatagram
  , niNoFullyQualifiedDomainName
  , niNumericHost
  , niNumericService
  ) where

import Control.Exception
import Control.Monad

import Data.Monoid
import Data.Bits
import Data.Typeable
import qualified Data.ByteString as BS

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc

import System.IO.Unsafe

import System.Socket.Family.Inet
import System.Socket.Family.Inet6
import System.Socket.Internal.Socket
import System.Socket.Internal.Platform

#include "hs_socket.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-------------------------------------------------------------------------------
-- AddressInfo
-------------------------------------------------------------------------------

data AddressInfo f t p
   = AddressInfo
     { addressInfoFlags :: AddressInfoFlags
     , socketAddress    :: SocketAddress f
     , canonicalName    :: Maybe BS.ByteString
     }

deriving instance (Eq   (SocketAddress f)) => Eq   (AddressInfo f t p)
deriving instance (Show (SocketAddress f)) => Show (AddressInfo f t p)

-------------------------------------------------------------------------------
-- AddressInfoException
-------------------------------------------------------------------------------

-- | Contains the error code that can be matched against. Use `show`
--   to get a human readable explanation of the error.
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
eaiAgain    :: AddressInfoException
eaiAgain     = AddressInfoException (#const EAI_AGAIN)

-- | > AddressInfoException "Bad value for ai_flags"
eaiBadFlags :: AddressInfoException
eaiBadFlags  = AddressInfoException (#const EAI_BADFLAGS)

-- | > AddressInfoException "Non-recoverable failure in name resolution"
eaiFail     :: AddressInfoException
eaiFail      = AddressInfoException (#const EAI_FAIL)

-- | > AddressInfoException "ai_family not supported"
eaiFamily   :: AddressInfoException
eaiFamily    = AddressInfoException (#const EAI_FAMILY)

-- | > AddressInfoException "Memory allocation failure"
eaiMemory   :: AddressInfoException
eaiMemory    = AddressInfoException (#const EAI_MEMORY)

-- | > AddressInfoException "No such host is known"
eaiNoName   :: AddressInfoException
eaiNoName    = AddressInfoException (#const EAI_NONAME)

-- | > AddressInfoException "Servname not supported for ai_socktype"
eaiService  :: AddressInfoException
eaiService   = AddressInfoException (#const EAI_SERVICE)

-- | > AddressInfoException "ai_socktype not supported"
eaiSocketType :: AddressInfoException
eaiSocketType  = AddressInfoException (#const EAI_SOCKTYPE)

-- | > AddressInfoException "System error"
eaiSystem   :: AddressInfoException
eaiSystem    = AddressInfoException (#const EAI_SYSTEM)

-- | Use the `Data.Monoid.Monoid` instance to combine several flags:
--
--   > mconcat [aiAddressConfig, aiV4Mapped]
newtype AddressInfoFlags
      = AddressInfoFlags CInt
      deriving (Eq, Show, Bits)

instance Data.Monoid.Monoid AddressInfoFlags where
  mempty
    = AddressInfoFlags 0
  mappend (AddressInfoFlags a) (AddressInfoFlags b)
    = AddressInfoFlags (a .|. b)

-- | @AI_ADDRCONFIG@:
aiAddressConfig  :: AddressInfoFlags
aiAddressConfig   = AddressInfoFlags (#const AI_ADDRCONFIG)

-- | @AI_ALL@: Return both IPv4 (as mapped `SocketAddressInet6`) and IPv6 addresses when
-- `aiV4Mapped` is set independent of whether IPv6 addresses exist for this
--  name.
aiAll             :: AddressInfoFlags
aiAll              = AddressInfoFlags (#const AI_ALL)

-- | @AI_CANONNAME@:
aiCanonicalName   :: AddressInfoFlags
aiCanonicalName    = AddressInfoFlags (#const AI_CANONNAME)

-- | @AI_NUMERICHOST@:
aiNumericHost     :: AddressInfoFlags
aiNumericHost      = AddressInfoFlags (#const AI_NUMERICHOST)

-- | @AI_NUMERICSERV@:
aiNumericService  :: AddressInfoFlags
aiNumericService   = AddressInfoFlags (#const AI_NUMERICSERV)

-- | @AI_PASSIVE@:
aiPassive         :: AddressInfoFlags
aiPassive          = AddressInfoFlags (#const AI_PASSIVE)

-- | @AI_V4MAPPED@: Return mapped IPv4 addresses if no IPv6 addresses could be found
--   or if `aiAll` flag is set.
aiV4Mapped        :: AddressInfoFlags
aiV4Mapped         = AddressInfoFlags (#const AI_V4MAPPED)

-- | Use the `Data.Monoid.Monoid` instance to combine several flags:
--
--   > mconcat [niNameRequired, niNoFullyQualifiedDomainName]
newtype NameInfoFlags
      = NameInfoFlags CInt
      deriving (Eq, Show, Bits)

instance Monoid NameInfoFlags where
  mempty
    = NameInfoFlags 0
  mappend (NameInfoFlags a) (NameInfoFlags b)
    = NameInfoFlags (a .|. b)

-- | @NI_NAMEREQD@: Throw an exception if the hostname cannot be determined.
niNameRequired               :: NameInfoFlags
niNameRequired                = NameInfoFlags (#const NI_NAMEREQD)

-- | @NI_DGRAM@: Service is datagram based (i.e. `System.Socket.Protocol.UDP.UDP`) rather than stream based (i.e. `System.Socket.Protocol.TCP.TCP`).
niDatagram                   :: NameInfoFlags
niDatagram                    = NameInfoFlags (#const NI_DGRAM)

-- | @NI_NOFQDN@: Return only the hostname part of the fully qualified domain name for local hosts.
niNoFullyQualifiedDomainName :: NameInfoFlags
niNoFullyQualifiedDomainName  = NameInfoFlags (#const NI_NOFQDN)

-- | @NI_NUMERICHOST@: Return the numeric form of the host address.
niNumericHost                :: NameInfoFlags
niNumericHost                 = NameInfoFlags (#const NI_NUMERICHOST)

-- | @NI_NUMERICSERV@: Return the numeric form of the service address.
niNumericService             :: NameInfoFlags
niNumericService              = NameInfoFlags (#const NI_NUMERICSERV)

class (Family f) => HasAddressInfo f where
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
--   `aiV4Mapped` and use IPv6-sockets.
--
--   > getAddressInfo (Just "www.haskell.org") (Just "https") mempty :: IO [AddressInfo Inet Stream TCP]
--   > > [AddressInfo {addressInfoFlags = AddressInfoFlags 0, socketAddress = SocketAddressInet {inetAddress = InetAddress 162.242.239.16, inetPort = InetPort 443}, canonicalName = Nothing}]
--
--   > > getAddressInfo (Just "www.haskell.org") (Just "80") aiV4Mapped :: IO [AddressInfo Inet6 Stream TCP]
--   > [AddressInfo {
--   >    addressInfoFlags = AddressInfoFlags 8,
--   >    socketAddress    = SocketAddressInet6 {inet6Address = Inet6Address 2400:cb00:2048:0001:0000:0000:6ca2:cc3c, inet6Port = Inet6Port 80, inet6FlowInfo = Inet6FlowInfo 0, inet6ScopeId = Inet6ScopeId 0},
--   >    canonicalName    = Nothing }]
--
--   > > getAddressInfo (Just "darcs.haskell.org") Nothing aiV4Mapped :: IO [AddressInfo Inet6 Stream TCP]
--   > [AddressInfo {
--   >    addressInfoFlags = AddressInfoFlags 8,
--   >    socketAddress    = SocketAddressInet6 {inet6Address = Inet6Address 0000:0000:0000:0000:0000:ffff:17fd:e1ad, inet6Port = Inet6Port 0, inet6FlowInfo = Inet6FlowInfo 0, inet6ScopeId = Inet6ScopeId 0},
--   >    canonicalName    = Nothing }]
--   > > getAddressInfo (Just "darcs.haskell.org") Nothing mempty :: IO [AddressInfo Inet6 Stream TCP]
--   > *** Exception: AddressInfoException "Name or service not known"
  getAddressInfo :: (Type t, Protocol p) => Maybe BS.ByteString -> Maybe BS.ByteString -> AddressInfoFlags -> IO [AddressInfo f t p]

instance HasAddressInfo Inet where
  getAddressInfo = getAddressInfo'

instance HasAddressInfo Inet6 where
  getAddressInfo = getAddressInfo'

getAddressInfo' :: forall f t p. (Family f, Storable (SocketAddress f), Type t, Protocol p) => Maybe BS.ByteString -> Maybe BS.ByteString -> AddressInfoFlags -> IO [AddressInfo f t p]
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

-- | A `NameInfo` consists of host and service name.
data NameInfo
   = NameInfo
     { hostName    :: BS.ByteString
     , serviceName :: BS.ByteString
     } deriving (Eq, Show)

-- | Maps addresses to readable host- and service names.
--
--   The operation throws `AddressInfoException`s.
--
--   > > getNameInfo (SocketAddressInet loopback 80) mempty
--   > NameInfo {hostName = "localhost.localdomain", serviceName = "http"}
class (Family f) => HasNameInfo f where
  getNameInfo :: SocketAddress f -> NameInfoFlags -> IO NameInfo

instance HasNameInfo Inet where
  getNameInfo = getNameInfo'

instance HasNameInfo Inet6 where
  getNameInfo = getNameInfo'

getNameInfo' :: Storable a => a -> NameInfoFlags -> IO NameInfo
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
          return $ NameInfo host serv
        else do
          throwIO (AddressInfoException e)
