{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Network.Database (
    AddrInfo (..)
  , AddrInfoFlags
  , getAddrInfo
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

import Data.Bits
import Data.Monoid
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

data AddrInfoException
   = EAI_ADDRFAMILY
   | EAI_AGAIN
   | EAI_BADFLAGS
   | EAI_FAIL
   | EAI_FAMILY
   | EAI_MEMORY
   | EAI_NODATA
   | EAI_NONAME
   | EAI_OVERFLOW
   | EAI_SERVICE
   | EAI_SOCKTYPE
   | EAI_SYSTEM
   deriving (Eq, Show)

newtype AddrInfoFlags
      = AddrInfoFlags CInt
      deriving (Eq, Show)

instance Monoid AddrInfoFlags where
  mempty
    = AddrInfoFlags 0
  mappend (AddrInfoFlags a) (AddrInfoFlags b)
    = AddrInfoFlags (a .|. b)

getAddrInfo :: forall a t p. (Address a, Type t, Protocol p) => Maybe BS.ByteString -> Maybe BS.ByteString -> AddrInfoFlags -> IO [AddrInfo a t p]
getAddrInfo mnode mservice (AddrInfoFlags flags) = do
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
            (\e-> do if e == 0 then do
                       resultPtr <- peek resultPtrPtr
                       peekAddrInfos resultPtr
                      else do
                        msgPtr <- c_gaistrerror e
                        msg <- peekCString msgPtr
                        throwIO (userError msg)
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

-------------------------------------------------------------------------------
-- FFI
-------------------------------------------------------------------------------

foreign import ccall safe "netdb.h getaddrinfo"
  c_getaddrinfo  :: CString -> CString -> Ptr (AddrInfo a t p) -> Ptr (Ptr (AddrInfo a t p)) -> IO CInt

foreign import ccall unsafe "netdb.h freeaddrinfo"
  c_freeaddrinfo :: Ptr (AddrInfo a t p) -> IO ()

foreign import ccall unsafe "netdb.h gai_strerror"
  c_gaistrerror  :: CInt -> IO CString

foreign import ccall unsafe "string.h memset"
  c_memset       :: Ptr a -> CInt -> CSize -> IO ()

