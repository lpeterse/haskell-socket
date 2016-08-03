#include <hs_socket.h>

int hs_get_last_socket_error() {
  return WSAGetLastError();
}

int hs_socket_init() {
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
  return 0;
};

int hs_socket(int domain, int type, int protocol, int *err) {
  if (!hs_socket_init()) {
    u_long iMode = 1;
    int fd = socket(domain, type, protocol);
    if (fd >= 0) {
      if (!ioctlsocket(fd, FIONBIO, &iMode)) {
        return fd;
      } else {
        closesocket(fd);
      }
    }
  }
  *err = WSAGetLastError();
  return -1;
};

int hs_bind(int sockfd, const struct sockaddr *name, int namelen, int *err) {
  int i = bind(sockfd, name, namelen);
  if (i) {
    *err = WSAGetLastError();
  }
  return i;
};

int hs_connect(int sockfd, const struct sockaddr *name, int namelen, int *err) {
  int i = connect(sockfd, name, namelen);
  if (i) {
    *err = WSAGetLastError();
  }
  return i;
};

/**
* Determine a sockets connection status.
*
* Return values:
*   0: connection established
*   1: connection pending
*   2: connection failed
*   3: select or getsockopt failed
*
* The operation will block for the least possible time interval (1 micro second)
* and is not used as it is supposed to be used as we don't want to block.
* Haskell's RTS is used to wait and poll this operation from time to time.
*/
int hs_connect_status (int sockfd, int *err) {
  int errlen = sizeof(int);
  struct timeval timeout = {0,1};
  fd_set writefds;
  fd_set exceptfds;

  FD_ZERO(&writefds);
  FD_ZERO(&exceptfds);
  FD_SET(sockfd, &writefds);
  FD_SET(sockfd, &exceptfds);

  switch (select(sockfd, NULL, &writefds, &exceptfds, &timeout)) {
    case 0:
      return 1;   // Connection pending.
    case 1:
      if (FD_ISSET(sockfd, &writefds)) {
        return 0; // Connection established.
      }
      if (FD_ISSET(sockfd, &exceptfds) && !getsockopt(sockfd, SOL_SOCKET, SO_ERROR, (char*) err, &errlen)) {
        return 2; // Connection failed.
      }
    default:
      *err = WSAGetLastError();
      return -1; // select or getsockopt failed.
  }
}

int hs_listen (int sockfd, int backlog, int *err) {
  int i = listen(sockfd, backlog);
  if (i) {
    *err = WSAGetLastError();
  }
  return i;
};

int hs_accept(int fd, struct sockaddr *addr, int *addrlen, int *err) {
  u_long iMode = 1;
  int ft = accept(fd, addr, addrlen);
  if (ft >= 0) {
    if (!ioctlsocket(ft, FIONBIO, &iMode)) {
      return ft;
    } else {
      closesocket(ft);
    }
  }
  *err = WSAGetLastError();
  return ft;
}

int hs_close(int sockfd) {
  return closesocket(sockfd);
};

int hs_send    (int sockfd, const void *buf, size_t len, int flags) {
  return send(sockfd, buf, len, flags);
};

int hs_recv    (int sockfd,       void *buf, size_t len, int flags) {
  return recv(sockfd, buf, len, flags);
};

int hs_sendto  (int sockfd, const void *buf, size_t len, int flags,
                const struct sockaddr *dest_addr, int addrlen) {
  return sendto(sockfd, buf, len, flags, dest_addr, addrlen);
};

int hs_recvfrom(int sockfd,       void *buf, size_t len, int flags,
                      struct sockaddr *src_addr, int *addrlen) {
  return recvfrom(sockfd, buf, len, flags, src_addr, addrlen);
};

int hs_getsockopt(int sockfd, int level, int option_name,       void *option_value, int *option_len) {
  return getsockopt(sockfd, level, option_name, option_value, option_len);
};

int hs_setsockopt(int sockfd, int level, int option_name, const void *option_value, int  option_len) {
  return setsockopt(sockfd, level, option_name, option_value, option_len);
};

const char *hs_gai_strerror(int errcode) {
  return gai_strerror(errcode);
};

int  hs_getaddrinfo(const char *node, const char *service,
                    const struct addrinfo *hints,
                    struct addrinfo **res) {
  if (hs_socket_init() != 0) {
    return -1;
  }
  return getaddrinfo(node, service, hints, res);
};

int  hs_getnameinfo(const struct sockaddr *sa, int salen,
                    char *host, int hostlen,
                    char *serv, int servlen, int flags) {
  if (hs_socket_init() != 0) {
    return -1;
  }
  return getnameinfo(sa, salen, host, hostlen, serv, servlen, flags);
};

void hs_freeaddrinfo(struct addrinfo *res) {
  freeaddrinfo(res);
  return;
};
