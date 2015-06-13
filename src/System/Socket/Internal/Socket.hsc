module System.Socket.Internal.Socket (
    Socket (..)
  , GetSockOpt (..)
  , getSockOptBool
  , SetSockOpt (..)
  , setSockOptBool
  , SO_ERROR (..)
  , SO_REUSEADDR (..)
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Applicative

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import System.Posix.Types

import System.Socket.Internal.Platform
import System.Socket.Internal.Exception

#include "hs_socket.h"

-- | A generic socket type. Also see `socket` for details.
--
--   The socket is just an `Control.Concurrent.MVar.MVar`-wrapped file descriptor.
--   It is exposed in order to make this library easily extensible, but it is
--   usually not necessary nor advised to work directly on the file descriptor.
--   If you do, the following rules must be obeyed:
--
--   - Make sure not to deadlock. Use `Control.Concurrent.MVar.withMVar` or similar.
--   - The lock __must not__ be held during a blocking call. This would make it impossible
--     to send and receive simultaneously or to close the socket.
--   - The lock __must__ be held when calling operations that use the file descriptor.
--     Otherwise the socket might get closed or even reused by another
--     thread/capability which might result in reading from or writing
--     totally different connection. This is a security nightmare!
--   - The socket is non-blocking and all the code relies on that assumption.
--     You need to use GHC's eventing mechanism primitives to block until
--     something happens. The former rules forbid to use `GHC.Conc.threadWaitRead` as it
--     does not seperate between registering the file descriptor (for which
--     the lock __must__ be held) and the actual waiting (for which you must
--     __not__ hold the lock).
--     Also see [this](https://mail.haskell.org/pipermail/haskell-cafe/2014-September/115823.html)
--     thread and read the library code to see how the problem is currently circumvented.
newtype Socket f t p
      = Socket (MVar Fd)

class GetSockOpt o where
  getSockOpt :: Socket f t p -> IO o

class SetSockOpt o where
  setSockOpt :: Socket f t p -> o -> IO ()

data SO_ERROR
   = SO_ERROR SocketException
   deriving (Eq, Ord, Show)

instance GetSockOpt SO_ERROR where
  getSockOpt s =
    SO_ERROR . SocketException <$> getSockOptCInt s (#const SOL_SOCKET) (#const SO_ERROR)

data SO_REUSEADDR
   = SO_REUSEADDR Bool
   deriving (Eq, Ord, Show)

instance GetSockOpt SO_REUSEADDR where
  getSockOpt s =
    SO_REUSEADDR <$> getSockOptBool s (#const SOL_SOCKET) (#const SO_REUSEADDR)

instance SetSockOpt SO_REUSEADDR where
  setSockOpt s (SO_REUSEADDR o) =
    setSockOptBool s (#const SOL_SOCKET) (#const SO_REUSEADDR) o

-------------------------------------------------------------------------------
-- Unsafe helpers
-------------------------------------------------------------------------------

setSockOptBool :: Socket f t p -> CInt -> CInt -> Bool -> IO ()
setSockOptBool (Socket mfd) level name value = do
  withMVar mfd $ \fd->
    alloca $ \vPtr-> do
        if value
          then poke vPtr 1
          else poke vPtr 0
        i <- c_setsockopt fd level name 
                          (vPtr :: Ptr CInt)
                          (fromIntegral $ sizeOf (undefined :: CInt))
        when (i < 0) $ do
          c_get_last_socket_error >>= throwIO

getSockOptBool :: Socket f t p -> CInt -> CInt -> IO Bool
getSockOptBool (Socket mfd) level name = do
  withMVar mfd $ \fd->
    alloca $ \vPtr-> do
      alloca $ \lPtr-> do
        i <- c_getsockopt fd level name 
                          (vPtr :: Ptr CInt)
                          (lPtr :: Ptr CInt)
        if i < 0 then do
          c_get_last_socket_error >>= throwIO
        else do
          v <- peek vPtr
          return (v == 1)

setSockOptInt :: Socket f t p -> CInt -> CInt -> Int -> IO ()
setSockOptInt (Socket mfd) level name value = do
  withMVar mfd $ \fd->
    alloca $ \vPtr-> do
        poke vPtr (fromIntegral value :: CInt)
        i <- c_setsockopt fd level name 
                          (vPtr :: Ptr CInt)
                          (fromIntegral $ sizeOf (undefined :: CInt))
        when (i < 0) $ do
          c_get_last_socket_error >>= throwIO

getSockOptInt :: Socket f t p -> CInt -> CInt -> IO Int
getSockOptInt (Socket mfd) level name = do
  withMVar mfd $ \fd->
    alloca $ \vPtr-> do
      alloca $ \lPtr-> do
        i <- c_getsockopt fd level name
                          (vPtr :: Ptr CInt)
                          (lPtr :: Ptr CInt)
        if i < 0 then do
          c_get_last_socket_error >>= throwIO
        else do
          v <- peek vPtr
          return (fromIntegral v)

setSockOptCInt :: Socket f t p -> CInt -> CInt -> CInt -> IO ()
setSockOptCInt (Socket mfd) level name value = do
  withMVar mfd $ \fd->
    alloca $ \vPtr-> do
        poke vPtr value
        i <- c_setsockopt fd level name 
                          (vPtr :: Ptr CInt)
                          (fromIntegral $ sizeOf (undefined :: CInt))
        when (i < 0) $ do
          c_get_last_socket_error >>= throwIO

getSockOptCInt :: Socket f t p -> CInt -> CInt -> IO CInt
getSockOptCInt (Socket mfd) level name = do
  withMVar mfd $ \fd->
    alloca $ \vPtr-> do
      alloca $ \lPtr-> do
        i <- c_getsockopt fd level name
                          (vPtr :: Ptr CInt)
                          (lPtr :: Ptr CInt)
        if i < 0 then do
          c_get_last_socket_error >>= throwIO
        else do
          peek vPtr
