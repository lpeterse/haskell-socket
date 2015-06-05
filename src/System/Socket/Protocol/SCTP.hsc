module System.Socket.Protocol.SCTP where

import System.Socket.Protocol

#include "hs_socket.h"

data SCTP

instance Protocol  SCTP where
  protocolNumber _ = (#const IPPROTO_SCTP)