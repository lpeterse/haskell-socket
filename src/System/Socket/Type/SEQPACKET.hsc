module System.Socket.Type.SEQPACKET where

import System.Socket.Type

#include "sys/socket.h"
#include "netinet/in.h"

data SEQPACKET

instance Type SEQPACKET where
  typeNumber _ = (#const SOCK_SEQPACKET)