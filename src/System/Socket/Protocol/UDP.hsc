module System.Socket.Protocol.UDP where

import System.Socket.Protocol

#include "sys/socket.h"
#include "netinet/in.h"

data UDP

instance Protocol  UDP where
  protocolNumber _ = (#const IPPROTO_UDP)