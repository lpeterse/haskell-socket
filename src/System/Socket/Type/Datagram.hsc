module System.Socket.Type.Datagram where

import System.Socket.Internal.Socket

#include "hs_socket.h"

data Datagram

instance Type Datagram where
  typeNumber _ = (#const SOCK_DGRAM)
