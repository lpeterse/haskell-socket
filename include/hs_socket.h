#ifndef _HS_SOCKET
#define _HS_SOCKET

#if defined(_WIN32)

#include <hs_socket_win32.h>

#else

#include <hs_socket_posix.h>

#endif /* defined(_WIN32) */
#endif /* _HS_SOCKET */

