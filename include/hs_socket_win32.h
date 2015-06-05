#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>

struct msghdr {
  void msg_name;
  void msg_namelen;
  void msg_iov;
  void msg_iovlen;
  void msg_flags;
};

struct iovec {
  void iov_base;
  void iov_len;
};