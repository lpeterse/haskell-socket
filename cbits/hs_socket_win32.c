#include <hs_socket.h>
#include <stdio.h>
#include <errno.h>

int hs_socket(int domain, int type, int protocol) {

  static int has_already_been_initialised = 0;

  if (!has_already_been_initialised) {
    WSADATA wsaData;

    int ini = WSAStartup(MAKEWORD(2,2), &wsaData);
    if (ini != NO_ERROR) {
      WSACleanup();
      return -1;
    } else {
      has_already_been_initialised = 1;
    }
  }

  return socket(domain, type, protocol);
};

int hs_connect(int sockfd, const struct sockaddr *name, int namelen) {
  return connect(sockfd, name, namelen);
};

int hs_close(int sockfd) {
  return closesocket(sockfd);
};

int setnonblocking(int fd) {
  // If iMode = 0, blocking is enabled; 
  // If iMode != 0, non-blocking mode is enabled.
  u_long iMode = 1;
  return ioctlsocket(fd, FIONBIO, &iMode);
};

int sendmsg(int sockfd, const struct msghdr *msg, int flags) {
  return -1;
};

int recvmsg(int sockfd,       struct msghdr *msg, int flags) {
  return -1;
};

int hs_get_last_socket_error(void) {
  return WSAGetLastError();
};
