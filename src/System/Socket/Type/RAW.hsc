module System.Socket.Type.RAW where

import System.Socket.Type

#include "hs_socket.h"

data RAW

instance Type RAW where
  typeNumber _ = (#const SOCK_RAW)