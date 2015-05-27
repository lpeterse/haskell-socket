module System.Socket.Type.DGRAM where

import System.Socket.Type

#include "sys/socket.h"
#include "netinet/in.h"

data DGRAM

instance Type DGRAM where
  typeNumber _ = (#const SOCK_DGRAM)