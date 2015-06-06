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

int hs_bind(int sockfd, const struct sockaddr *name, int namelen) {
  return bind(sockfd, name, namelen);
};

int hs_connect(int sockfd, const struct sockaddr *name, int namelen) {
  int i = connect(sockfd, name, namelen);
  if (i != 0) {
    switch (WSAGetLastError()) {
      case WSAEWOULDBLOCK:
        return 0;
        break;
      // TODO: remap other error codes that don't behave the posix way.
      default:
        break;
    }
  }
  return i;
};

int hs_listen (int sockfd, int backlog) {
  return listen(sockfd, backlog);
};

int hs_accept(int sockfd, struct sockaddr *addr, int *addrlen) {
  return accept(sockfd, addr, addrlen);
}

int hs_close(int sockfd) {
  return closesocket(sockfd);
};

int setnonblocking(int fd) {
  // If iMode = 0, blocking is enabled; 
  // If iMode != 0, non-blocking mode is enabled.
  u_long iMode = 1;
  //return ioctlsocket(fd, FIONBIO, &iMode);
  return 0;
};

int hs_send    (int sockfd, const void *buf, size_t len, int flags) {
  printf("send1");
  int x = send(sockfd, buf, len, flags);
  printf("send2\n");
  printf("%ld\n", x);
  return x;
};

int hs_recv    (int sockfd,       void *buf, size_t len, int flags) {
  return -1;
};

int hs_sendto  (int sockfd, const void *buf, size_t len, int flags,
                const struct sockaddr *dest_addr, int addrlen) {
  return -1;
};

int hs_recvfrom(int sockfd,       void *buf, size_t len, int flags,
                      struct sockaddr *src_addr, int *addrlen) {
  return -1;
};

int hs_sendmsg(int sockfd, const struct msghdr *msg, int flags) {
  return -1;
};

int hs_recvmsg(int sockfd,       struct msghdr *msg, int flags) {
  return -1;
};

int hs_get_last_socket_error(void) {
  return WSAGetLastError();
};

int hs_getsockopt(int sockfd, int level, int option_name,       void *option_value, int *option_len) {
  return getsockopt(sockfd, level, option_name, option_value, option_len);
};

int hs_setsockopt(int sockfd, int level, int option_name, const void *option_value, int  option_len) {
  return setsockopt(sockfd, level, option_name, option_value, option_len);
};