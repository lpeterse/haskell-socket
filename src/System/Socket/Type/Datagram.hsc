module System.Socket.Type.Datagram where

import System.Socket.Type

#include "hs_socket.h"

data Datagram

instance Type Datagram where
  typeNumber _ = (#const SOCK_DGRAM)