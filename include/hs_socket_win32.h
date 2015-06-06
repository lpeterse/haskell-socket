#include <stdint.h>
#include <windows.h>
#include <winsock2.h>
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

int sendmsg(int sockfd, const struct msghdr *msg, int flags);
int recvmsg(int sockfd,       struct msghdr *msg, int flags);