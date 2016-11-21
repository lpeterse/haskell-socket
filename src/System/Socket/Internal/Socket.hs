{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Internal.Socket (
    Socket (..)
  , SocketAddress
  , Family (..)
  , Type (..)
  , Protocol (..)
  ) where

import           Control.Concurrent.MVar
import           Foreign.C.Types
import           System.Posix.Types

-- | A generic socket type. Use `System.Socket.socket` to create a new socket.
--
--   The socket is just an `Control.Concurrent.MVar.MVar`-wrapped file descriptor.
--   The `System.Socket.Unsafe.Socket` constructor is exported trough the unsafe
--   module in order to make  this library easily extensible, but it is usually
--   not necessary nor advised to work directly on the file descriptor.
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
--     does not separate between registering the file descriptor (for which
--     the lock __must__ be held) and the actual waiting (for which you must
--     __not__ hold the lock).
--     Also see [this](https://mail.haskell.org/pipermail/haskell-cafe/2014-September/115823.html)
--     thread and read the library code to see how the problem is currently circumvented.
newtype Socket f t p
      = Socket (MVar Fd)

-- | The `SocketAddress` type is a [data family](https://wiki.haskell.org/GHC/Type_families#Detailed_definition_of_data_families).
--   This allows to provide different data constructors depending on the socket
--   family wihtout knowing all of them in advance or the need to patch this
--   core library.
--
-- > SocketAddressInet  inetLoopback  8080     :: SocketAddress Inet
-- > SocketAddressInet6 inet6Loopback 8080 0 0 :: SocketAddress Inet6
data family SocketAddress f

class Family f where
  familyNumber :: f -> CInt

class Type t where
  typeNumber :: t -> CInt

class Protocol  p where
  protocolNumber :: p -> CInt
