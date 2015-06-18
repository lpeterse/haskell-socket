module System.Socket.Type.SequentialPacket where

import System.Socket.Type

#include "hs_socket.h"

data SequentialPacket

instance Type SequentialPacket where
  typeNumber _ = (#const SOCK_SEQPACKET)