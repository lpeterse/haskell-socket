 * Added convenience operation `withConnection`.
 * Added `eNETUNREACH` exception constant.
 * Added new operation `recvAll` and changed `sendAll` to lazy `ByteString`.
 * Added new socket option IPV6_V6ONLY.
 * Removed untested socket option SO_ACCEPTCONN.
 * Correctly defining AI_ flags on Windows (MinGW doesn't although
   they are all well support on Vista or higher).
 * Got all tests passing on Windows 7.

0.3.0.1 Lars Petersen <info@lars-petersen.net> 2015-06-07

 * Fixed documentation of eaiNONAME.
 * Fixed typo in .cabal file in reference to cbits file.

0.3.0.0 Lars Petersen <info@lars-petersen.net> 2015-06-07

 * `AddrInfoFlags` and `NameInfoFlags` are now instances of `Bits`.
 * Dropped all sendmsg/recvmsg related operations (harden the core first!)
 * Dropped support for UNIX socket (will be separate package `socket-unix`)
 * Renamed type function `Address` to `SockAddr`.
 * Added GetAddrInfo and GetNameInfo classes.
 * Dropped support for SCTP (will be separate package `socket-sctp`)
 * Added support for RAW sockets.
 * Started to support Windows (still unfinished).
 * New operation `recvRecord`.
 * ReceiveMsg now returns a strict `ByteString`.
 * New operations `sendV`, `sendToV`.
 * Restricted getAddrInfo and getNameInfo and added `getAddrInfo6` and
   `getNameInfo6`
 * Added address family types INET, INET6 and UNIX (API breaking change)
 * Hide `SockAddrIn6` address constructor
 * Hide `SockAddrIn` address constructor
 * Added `recvMsg` operation
 * Fixed unsafeSend, unsafeSendTo and unsafeSendMsg (they were waiting for
   a read event instead of waiting for writing)
 * Use `aiStrError` values in Show instance
 * Added `aiStrError` function
 * Added constants for AddrInfoException
 * Changed definitin of AddrInfoException
 * Added `sendAllMsg` operation
 * Added `sendMsg` operation (+ some types and internals)

0.2.0.0 Lars Petersen <info@lars-petersen.net> 2015-05-29

 * Added a sendAll operation
 * Exposed the Socket constructor
 * Added `getNameInfo` operation
 * Added msgWAITALL and fixed serious bug regarding all other MsgFlags
 * Nicer Show instances for SockAddrIn and SockAddrIn6
 * Hiding internal modules
 * Added `getAddrInfo` operation

0.1.0.1 Lars Petersen <info@lars-petersen.net> 2015-05-28

 * Added CHANGELOG.md
 * Removed `threadWaitReadMVar` and `threadWaitWriteMVar`
 * Import `Data.Monoid` in `System.Socket.Unsafe` to support older Preludes

0.1.0.0 Lars Petersen <info@lars-petersen.net> 2015-05-28

 * Initial release