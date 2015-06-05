module System.Socket.Type.SEQPACKET where

import System.Socket.Type

#include "hs_socket.h"

data SEQPACKET

instance Type SEQPACKET where
  typeNumber _ = (#const SOCK_SEQPACKET)