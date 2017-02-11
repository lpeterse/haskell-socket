#define _GNU_SOURCE

#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include "sys/types.h"
#include "sys/socket.h"
#include "sys/un.h"
#include "netinet/in.h"
#include <netinet/tcp.h>
#include "netdb.h"

int hs_socket  (int domain, int type, int protocol, int *err);
int hs_connect (int fd, const struct sockaddr *name, int namelen, int *err);
int hs_bind    (int fd, const struct sockaddr *name, int namelen, int *err);
int hs_listen  (int fd, int backlog, int *err);
int hs_accept  (int fd, struct sockaddr *addr, int *addrlen, int *err);
int hs_close   (int fd, int *err);

int hs_send    (int fd, const void *buf, size_t len, int flags, int *err);
int hs_recv    (int fd,       void *buf, size_t len, int flags, int *err);
int hs_sendto  (int fd, const void *buf, size_t len, int flags, const struct sockaddr *dest_addr, int addrlen, int *err);
int hs_recvfrom(int fd,       void *buf, size_t len, int flags, struct sockaddr *src_addr, int *addrlen, int *err);

int hs_getsockopt(int fd, int level, int option_name,       void *option_value, int *option_len, int *err);
int hs_setsockopt(int fd, int level, int option_name, const void *option_value, int  option_len, int *err);

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
#define SECONNABORTED          ECONNABORTED
#define SECONNRESET            ECONNRESET
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
