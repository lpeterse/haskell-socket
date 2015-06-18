module System.Socket.Type.Stream where

import System.Socket.Type

#include "hs_socket.h"

data Stream

instance Type Stream where
  typeNumber _ = (#const SOCK_STREAM)