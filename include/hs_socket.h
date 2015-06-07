#ifndef _HS_SOCKET
#define _HS_SOCKET

#if defined(_WIN32)

#include <hs_socket_win32.h>

#else

#include <hs_socket_posix.h>

#endif /* defined(_WIN32) */

/* Flags (by convention) get defined 0 if they are not defined
   on the system. This has to be checked for in the application.
   This is the price to pay for not having conditionally defined
   symbols on different systems.
*/

#ifndef MSG_EOR
#define MSG_EOR 0
#endif

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

#ifndef MSG_WAITALL
#define MSG_WAITALL 0
#endif

#ifndef MSG_MORE
#define MSG_MORE 0
#endif

#ifndef MSG_TRUNC
#define MSG_TRUNC 0
#endif

#ifndef MSG_DONTWAIT
#define MSG_DONTWAIT 0
#endif

#ifndef EAI_SYSTEM
#define EAI_SYSTEM 0
#endif

#ifndef AI_ADDRCONFIG
#define AI_ADDRCONFIG 0
#endif

#ifndef AI_ALL
#define AI_ALL 0
#endif

#ifndef AI_NUMERICSERV
#define AI_NUMERICSERV 0
#endif

#ifndef AI_V4MAPPED
#define AI_V4MAPPED 0
#endif

#endif /* _HS_SOCKET */

