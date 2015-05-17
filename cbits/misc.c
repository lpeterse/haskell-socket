#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>

void poke_sockaddr_in6 ( struct sockaddr_in6 *addr
                   , uint16_t port
                   , uint32_t flowinfo
                   , uint8_t addr_0
                   , uint8_t addr_1
                   , uint8_t addr_2
                   , uint8_t addr_3
                   , uint8_t addr_4
                   , uint8_t addr_5
                   , uint8_t addr_6
                   , uint8_t addr_7
                   , uint8_t addr_8
                   , uint8_t addr_9
                   , uint8_t addr_A
                   , uint8_t addr_B
                   , uint8_t addr_C
                   , uint8_t addr_D
                   , uint8_t addr_E
                   , uint8_t addr_F
                   , uint32_t scope_id
                   ) {
  (*addr).sin6_family             = AF_INET6;
  (*addr).sin6_port               = htons(port);
  (*addr).sin6_flowinfo           = flowinfo;
  (*addr).sin6_addr.s6_addr[0x00] = addr_0;
  (*addr).sin6_addr.s6_addr[0x01] = addr_1;
  (*addr).sin6_addr.s6_addr[0x02] = addr_2;
  (*addr).sin6_addr.s6_addr[0x03] = addr_3;
  (*addr).sin6_addr.s6_addr[0x04] = addr_4;
  (*addr).sin6_addr.s6_addr[0x05] = addr_5;
  (*addr).sin6_addr.s6_addr[0x06] = addr_6;
  (*addr).sin6_addr.s6_addr[0x07] = addr_7;
  (*addr).sin6_addr.s6_addr[0x08] = addr_8;
  (*addr).sin6_addr.s6_addr[0x09] = addr_9;
  (*addr).sin6_addr.s6_addr[0x0a] = addr_A;
  (*addr).sin6_addr.s6_addr[0x0b] = addr_B;
  (*addr).sin6_addr.s6_addr[0x0c] = addr_C;
  (*addr).sin6_addr.s6_addr[0x0d] = addr_D;
  (*addr).sin6_addr.s6_addr[0x0e] = addr_E;
  (*addr).sin6_addr.s6_addr[0x0f] = addr_F;
  (*addr).sin6_scope_id           = scope_id;

  return;
}