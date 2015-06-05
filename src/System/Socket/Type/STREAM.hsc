module System.Socket.Type.STREAM where

import System.Socket.Type

#include "hs_socket.h"

data STREAM

instance Type STREAM where
  typeNumber _ = (#const SOCK_STREAM)