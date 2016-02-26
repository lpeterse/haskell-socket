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
#define SENETUNREACH           ENETUNREACH
#define SENOTCONN              ENOTCONN
#define SEALREADY              EALREADY
#define SEISCONN               EISCONN
#define SETIMEDOUT             ETIMEDOUT
#define SEPIPE                 EPIPE
#define SEOPNOTSUPP            EOPNOTSUPP
#define SENOTSOCK              ENOTSOCK
#define SEDESTADDRREQ          EDESTADDRREQ
#define SEMSGSIZE              EMSGSIZE
#define SEPROTOTYPE            EPROTOTYPE
#define SENOPROTOOPT           ENOPROTOOPT
#define SESOCKTNOSUPPORT       ESOCKTNOSUPPORT
#define SEPFNOSUPPORT          EPFNOSUPPORT
#define SEAFNOSUPPORT          EAFNOSUPPORT
#define SEADDRINUSE            EADDRINUSE
#define SEADDRNOTAVAIL         EADDRNOTAVAIL
#define SENETDOWN              ENETDOWN
#define SENETRESET             ENETRESET
#define SENOBUFS               ENOBUFS
#define SESHUTDOWN             ESHUTDOWN
#define SETOOMANYREFS          ETOOMANYREFS
#define SEHOSTDOWN             EHOSTDOWN
#define SEHOSTUNREACH          EHOSTUNREACH

/* MSG_NOSIGNAL might not be available (i.e. on MacOSX and Solaris).
 *   In this case it gets defined as 0. This is relatively
 *   safe to do as the GHC runtime ignores signals that aren't hooked.
 *   The application won't die, but might be unncessarily interrupted.
 */
#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL           0
#endif
