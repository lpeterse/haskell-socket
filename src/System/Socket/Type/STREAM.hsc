module System.Socket.Type.STREAM where

import System.Socket.Type

#include "sys/socket.h"
#include "netinet/in.h"

data STREAM

instance Type STREAM where
  typeNumber _ = (#const SOCK_STREAM)