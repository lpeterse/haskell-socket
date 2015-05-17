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
                   );