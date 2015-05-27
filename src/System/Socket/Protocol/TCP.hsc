module System.Socket.Protocol.TCP where

import System.Socket.Protocol

#include "sys/socket.h"
#include "netinet/in.h"

data TCP

instance Protocol  TCP where
  protocolNumber _ = (#const IPPROTO_TCP)