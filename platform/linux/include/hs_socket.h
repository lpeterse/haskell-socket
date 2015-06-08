#include <stdint.h>
#include <fcntl.h>
#include <errno.h>

#include "sys/types.h"
#include "sys/socket.h"
#include "sys/un.h"
#include "netinet/in.h"
#include "netdb.h"

/* SocketException */

int hs_setnonblocking(int fd);
int hs_get_last_socket_error(void);

#define SEOK                   0
#define SEINTR                 EINTR
#define SEAGAIN                EAGAIN
#define SEWOULDBLOCK           EWOULDBLOCK
#define SEBADF                 EBADF
#define SEINVAL                EINVAL
#define SEINPROGRESS           EINPROGRESS
#define SEPROTONOSUPPORT       EPROTONOSUPPORT
#define SECONNREFUSED          ECONNREFUSED
