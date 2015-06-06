#include <hs_socket.h>
#include <stdio.h>
#include <errno.h>

int hs_socket(int domain, int type, int protocol) {

  WSADATA wsaData;

  int iResult = WSAStartup(MAKEWORD(2,2), &wsaData);
  if (iResult != NO_ERROR) {
    return iResult;
  }

  int s = socket(domain, type, protocol);
  if (s == INVALID_SOCKET) {
    WSACleanup();
  }

  return s;
};

int hs_close(int sockfd) {
  return closesocket(sockfd);
}

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