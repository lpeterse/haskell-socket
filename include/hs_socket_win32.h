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

typedef u_short sa_family_t;

struct msghdr {
  void *msg_name;
  void *msg_namelen;
  void *msg_iov;
  void *msg_iovlen;
  void *msg_flags;
};

struct iovec {
  void *iov_base;
  void *iov_len;
};

struct sockaddr_un {
  sa_family_t sun_family;
  char        sun_path[108];
};

int hs_socket_init();

int hs_socket  (int domain, int type, int protocol);
int hs_bind    (int sockfd, const struct sockaddr *name, int namelen);
int hs_connect (int sockfd, const struct sockaddr *name, int namelen);
int hs_listen  (int sockfd, int backlog);
int hs_accept  (int sockfd, struct sockaddr *addr, int *addrlen);
int hs_close   (int sockfd);

int hs_send    (int sockfd, const void *buf, size_t len, int flags);
int hs_recv    (int sockfd,       void *buf, size_t len, int flags);
int hs_sendto  (int sockfd, const void *buf, size_t len, int flags,
                const struct sockaddr *dest_addr, int addrlen);
int hs_recvfrom(int sockfd,       void *buf, size_t len, int flags,
                      struct sockaddr *src_addr, int *addrlen);
int hs_sendmsg (int sockfd, const struct msghdr *msg, int flags);
int hs_recvmsg (int sockfd,       struct msghdr *msg, int flags);

int hs_getsockopt(int sockfd, int level, int option_name,       void *option_value, int *option_len);
int hs_setsockopt(int sockfd, int level, int option_name, const void *option_value, int  option_len);

int  hs_getaddrinfo(const char *node, const char *service,
                    const struct addrinfo *hints,
                    struct addrinfo **res);

int  hs_getnameinfo(const struct sockaddr *sa, int salen,
                    char *host, int hostlen,
                    char *serv, int servlen, int flags);

void hs_freeaddrinfo(struct addrinfo *res);

const char *hs_gai_strerror(int errcode);

/* SocketException */

int hs_get_last_socket_error(void);

#define SEOK                   0
#define SEINTR                 WSAEINTR
#define SEAGAIN                WSATRY_AGAIN
#define SEWOULDBLOCK           WSAEWOULDBLOCK
#define SEBADF                 WSAEBADF
#define SEINVAL                WSAEINVAL
#define SEINPROGRESS           WSAEINPROGRESS
#define SEPROTONOSUPPORT       WSAEPROTONOSUPPORT
#define SECONNREFUSED          WSAECONNREFUSED
