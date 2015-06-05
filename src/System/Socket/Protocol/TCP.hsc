module System.Socket.Protocol.TCP where

import System.Socket.Protocol

#include "hs_socket.h"

data TCP

instance Protocol  TCP where
  protocolNumber _ = (#const IPPROTO_TCP)