{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Internal.Socket
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Internal.Socket (
    Socket (..)
  , Family (..)
  , Type (..)
  , Protocol (..)
  ) where

import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Data.Typeable
import           Foreign.C.Types
import           Foreign.Storable
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
--     thread/capability which might result in reading from or writing to a
--     totally different socket. This is a security nightmare!
--   - The socket is non-blocking and all the code relies on that assumption.
--     You need to use GHC's eventing mechanism primitives to block until
--     something happens. The former rules forbid to use `GHC.Conc.threadWaitRead` as it
--     does not separate between registering the file descriptor (for which
--     the lock __must__ be held) and the actual waiting (for which you must
--     __not__ hold the lock).
--     Also see [this](https://mail.haskell.org/pipermail/haskell-cafe/2014-September/115823.html)
--     thread and read the library code to see how the problem is currently circumvented.
newtype Socket f t p
      = Socket (MVar Fd) deriving (Typeable)

instance Eq (Socket f t p) where
  (Socket x) == (Socket y) = x == y

instance NFData (Socket f t p) where
  rnf s = s `seq` ()

-- | The address `Family` determines the network layer to use.
--
--   The most common address families are `System.Socket.Family.Inet` (IPv4)
--   and `System.Socket.Family.Inet6` (IPv6).
class (Storable (SocketAddress f), NFData (SocketAddress f), Typeable (SocketAddress f), Typeable f) => Family f where
  -- | The number designating this `Family` on the specific platform. This
  --   method is only exported for implementing extension libraries.
  --
  --   This function shall yield the values of constants like `AF_INET`, `AF_INET6` etc.
  familyNumber :: f -> CInt
  -- | The `SocketAddress` type is a [data family](https://wiki.haskell.org/GHC/Type_families#Detailed_definition_of_data_families).
  --   This allows to provide different data constructors depending on the socket
  --   family without knowing all of them in advance or the need to extend this
  --   core library.
  --
  -- > SocketAddressInet  inetLoopback  8080     :: SocketAddress Inet
  -- > SocketAddressInet6 inet6Loopback 8080 0 0 :: SocketAddress Inet6
  data SocketAddress f

-- | The `Type` determines properties of the transport layer and the semantics
--   of basic socket operations.
--
--   The instances supplied by this library are `System.Socket.Type.Raw`
--   (no transport layer), `System.Socket.Type.Stream`
--   (for unframed binary streams, e.g. `System.Socket.Protocol.TCP`),
--   `System.Socket.Type.Datagram` (for datagrams
--   of limited length, e.g. `System.Socket.Protocol.UDP`) and
--   `System.Socket.Type.SequentialPacket` (for framed messages of arbitrary
--   length, e.g. `System.Socket.Protocol.SCTP`).
class (Typeable t) => Type t where
  -- | This number designates this `Type` on the specific platform. This
  --   method is only exported for implementing extension libraries.
  --
  --   The function shall yield the values of constants like `SOCK_STREAM`,
  --   `SOCK_DGRAM` etc.
  typeNumber :: t -> CInt

-- | The `Protocol` determines the transport protocol to use.
--
--   Use `System.Socket.Protocol.Default` to let the operating system choose
--   a transport protocol compatible with the socket's `Type`.
class (Typeable p) => Protocol  p where
  -- | This number designates this `Protocol` on the specific platform. This
  --   method is only exported for implementing extension libraries.
  --
  --   The function shall yield the values of constants like `IPPROTO_TCP`,
  --   `IPPROTO_UDP` etc.
  protocolNumber :: p -> CInt
