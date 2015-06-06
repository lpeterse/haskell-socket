#include <hs_socket.h>

int setnonblocking(int fd) {
  int flags;

  if (-1 == (flags = fcntl(fd, F_GETFL, 0))) {
        flags = 0;
  }
  return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}