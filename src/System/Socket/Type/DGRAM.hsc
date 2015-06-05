module System.Socket.Type.DGRAM where

import System.Socket.Type

#include "hs_socket.h"

data DGRAM

instance Type DGRAM where
  typeNumber _ = (#const SOCK_DGRAM)