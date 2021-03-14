#include <stdint.h>
#include <stdio.h>
#include <winsock2.h>

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif
#if (_WIN32_WINNT < 0x0501)
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif

#include <ws2tcpip.h>

#ifndef MSG_EOR
#define MSG_EOR 0
#endif

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

#ifndef MSG_WAITALL
#define MSG_WAITALL 0
#endif

#ifndef EAI_SYSTEM
#define EAI_SYSTEM 0
#endif

/* Value for this flag taken from the former Haskell network
   library. Remove if MinGW fixed this.
*/
#ifndef IPV6_V6ONLY
#define IPV6_V6ONLY 27
#endif

/* Values for this flags taken from
   http://sourceforge.net/p/mingw-w64/mailman/message/33056995/.
   According to MSDN documentation they are supported on
   Windows Vista or higher. This definitions may be removed
   when MinGW finally ships them.
*/

#ifndef AI_PASSIVE
#define AI_PASSIVE                  0x00000001
#endif
#ifndef AI_CANONNAME
#define AI_CANONNAME                0x00000002
#endif
#ifndef AI_NUMERICHOST
#define AI_NUMERICHOST              0x00000004
#endif
#ifndef AI_NUMERICSERV
#define AI_NUMERICSERV              0x00000008
#endif
#ifndef AI_ALL
#define AI_ALL                      0x00000100
#endif
#ifndef AI_ADDRCONFIG
#define AI_ADDRCONFIG               0x00000400
#endif
#ifndef AI_V4MAPPED
#define AI_V4MAPPED                 0x00000800
#endif
#ifndef AI_NON_AUTHORITATIVE
#define AI_NON_AUTHORITATIVE        0x00004000
#endif
#ifndef AI_SECURE
#define AI_SECURE                   0x00008000
#endif
#ifndef AI_RETURN_PREFERRED_NAMES
#define AI_RETURN_PREFERRED_NAMES   0x00010000
#endif

int hs_get_last_socket_error();

int hs_socket_init();

int hs_socket  (int domain, int type, int protocol, int *err);
int hs_bind    (int sockfd, const struct sockaddr *name, int namelen, int *err);
int hs_connect (int sockfd, const struct sockaddr *name, int namelen, int *err);
int hs_connect_status (int sockfd, int *err);
int hs_listen  (int sockfd, int backlog, int *err);
int hs_accept  (int sockfd, struct sockaddr *addr, int *addrlen, int *err);
int hs_close   (int sockfd, int *err);

int hs_send    (int sockfd, const void *buf, size_t len, int flags, int *err);
int hs_recv    (int sockfd,       void *buf, size_t len, int flags, int *err);
int hs_sendto  (int sockfd, const void *buf, size_t len, int flags, const struct sockaddr *dest_addr, int addrlen, int *err);
int hs_recvfrom(int sockfd,       void *buf, size_t len, int flags, struct sockaddr *src_addr, int *addrlen, int *err);

int hs_getsockopt(int sockfd, int level, int option_name,       void *option_value, int *option_len, int *err);
int hs_setsockopt(int sockfd, int level, int option_name, const void *option_value, int  option_len, int *err);

int  hs_getaddrinfo(const char *node, const char *service,
                    const struct addrinfo *hints,
                    struct addrinfo **res);

int  hs_getnameinfo(const struct sockaddr *sa, int salen,
                    char *host, int hostlen,
                    char *serv, int servlen, int flags);

void hs_freeaddrinfo(struct addrinfo *res);

int hs_shutdown(int fd, int how, int *err);

#define SEOK                   0
#define SEINTR                 WSAEINTR
#define SEAGAIN                WSATRY_AGAIN
#define SEWOULDBLOCK           WSAEWOULDBLOCK
#define SEBADF                 WSAEBADF
#define SEACCES                WSAEACCES
#define SEINVAL                WSAEINVAL
#define SEINPROGRESS           WSAEINPROGRESS
#define SEPROTONOSUPPORT       WSAEPROTONOSUPPORT
#define SECONNREFUSED          WSAECONNREFUSED
#define SENETUNREACH           WSAENETUNREACH
#define SENOTCONN              WSAENOTCONN
#define SEALREADY              WSAEALREADY
#define SEISCONN               WSAEISCONN
#define SETIMEDOUT             WSAETIMEDOUT
#define SEPIPE                 WSAECONNABORTED
#define SEOPNOTSUPP            WSAEOPNOTSUPP
#define SENOTSOCK              WSAENOTSOCK
#define SEHOSTUNREACH          WSAEHOSTUNREACH
#define SEHOSTDOWN             WSAEHOSTDOWN
#define SETOOMANYREFS          WSAETOOMANYREFS
#define SESHUTDOWN             WSAESHUTDOWN
#define SENOBUFS               WSAENOBUFS
#define SENETRESET             WSAENETRESET
#define SENETDOWN              WSAENETDOWN
#define SECONNABORTED          WSAECONNABORTED
#define SECONNRESET            WSAECONNRESET
#define SEADDRNOTAVAIL         WSAEADDRNOTAVAIL
#define SEADDRINUSE            WSAEADDRINUSE
#define SEAFNOSUPPORT          WSAEAFNOSUPPORT
#define SEPFNOSUPPORT          WSAEPFNOSUPPORT
#define SESOCKTNOSUPPORT       WSAESOCKTNOSUPPORT
#define SENOPROTOOPT           WSAENOPROTOOPT
#define SEPROTOTYPE            WSAEPROTOTYPE
#define SEMSGSIZE              WSAEMSGSIZE
#define SEDESTADDRREQ          WSAEDESTADDRREQ
