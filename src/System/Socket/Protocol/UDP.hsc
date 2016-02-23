module System.Socket.Protocol.UDP where

import System.Socket.Internal.Socket

#include "hs_socket.h"

data UDP

instance Protocol  UDP where
  protocolNumber _ = (#const IPPROTO_UDP)
