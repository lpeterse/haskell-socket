#include <hs_socket.h>

int hs_setnonblocking(int fd) {
  int flags;

  if (-1 == (flags = fcntl(fd, F_GETFL, 0))) {
        flags = 0;
  }
  return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

int hs_get_last_socket_error(void) {
  return errno;
};

int hs_socket (int domain, int type, int protocol, int *err) {
#ifdef SOCK_NONBLOCK
  // On Linux, there is an optimized way to set a socket non-blocking
  int fd = socket(domain, type | SOCK_NONBLOCK, protocol);
  if (fd >= 0) {
    return fd;
  }
#else
  // This is the regular way via fcntl
  int fd = socket(domain, type, protocol);
  if (fd >= 0) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags >= 0 && !fcntl(fd, F_SETFL, flags | O_NONBLOCK)) {
      return fd;
    } else {
      close(fd);
    }
  }
#endif
  *err = errno;
  return -1;
}

int hs_connect (int sockfd, const struct sockaddr *name, int namelen, int *err) {
  int i = connect(sockfd, name, namelen);
  if (i) {
    *err = errno;
  }
  return i;
}
