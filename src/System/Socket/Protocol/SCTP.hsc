module System.Socket.Protocol.SCTP where

import System.Socket.Protocol

#include "sys/socket.h"
#include "netinet/in.h"

data SCTP

instance Protocol  SCTP where
  protocolNumber _ = (#const IPPROTO_SCTP)