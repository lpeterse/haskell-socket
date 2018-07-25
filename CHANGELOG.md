0.9.0.0 Lars Petersen <info@lars-petersen.net> 2018-07-25

 * Issue 51: Add a `getAddress` operation for getting the local socket address.

0.8.0.1 Lars Petersen <info@lars-petersen.net> 2017-02-25

 * Issue 47: Fixed haddock documentation. 
 * Issue 46: Export `KeepAlive` socket option in the main module.

0.8.0.0 Lars Petersen <info@lars-petersen.net> 2017-02-25

 * Issue 26: Show instances for `SocketException` and `AddressInfoException`
   no longer use `strerror()` and `gai_strerror()`, but simply show the name
   of the error code. Linking to `gai_strerror()` was problematic on Windows
   and `strerror()` was not thread-safe on Linux.

 * Issue 38: Added SO_KEEPALIVE as `System.Socket.KeepAlive`.

 * Issue 37: Added TCP_NODELAY as `System.Socket.Protocol.TCP.NoDelay`.

 * Issue 40: Changed allocation function to malloPlainForeignPtr.
   This mechanism used in the ByteString library is heavily optimised and
   2 - 2.5 times faster than the previous implementation.
   See https://github.com/lpeterse/haskell-socket/pull/40 for details.

 * Issue 31: Added a `Default` protocol type.

 * Issue 27: Fixed a memory leak that manifested when interrupting threads
   waiting on socket events.

 * Issue 25: The `awaitEvent: invalid argument` was caused by the forked-off
   thread in `threadWaitRead`. The new code introduced by the changes caused
   by issue 27 catches and swallows this exception in a sane way.

 * Renamed `unsafeSocketWaitRead/Write/Connected` to `waitRead/Write/Connected`
   as these operations are not really unsafe, just internal. The operation
   signatures changed due to issue 27. Also, documented Windows specific
   implementation details.

 * The `connect` operation does no longer hold the lock on the socket while
   waiting for connection establishment.

 * Refactored and adapted the `accept` operation for changes caused by issue 27.
   Operation semantics shouldn't have changed.

 * Made `SocketAddress` an associated data family and added a
   `Storable (SocketAddress f)` constraint to the `Family` class. This allows
   omitting the `Storable` in socket operations (`Family` suffices now). It
   should be compatible with all existing code and not require any changes.

0.7.0.0 Lars Petersen <info@lars-petersen.net> 2016-11-13

 * Added function `sendAllLazy` and `sendAllBuilder`. Changed the signature and
   semantics of `sendAll` (thus the major version bump).

 * The `MessageFlags` constructor is now exported (kudos to Shea Levy for noting
   that this is necessary when writing extension libraries).

 * GHC 8 introduces a built-in alignment macro which is now used when present.
   This prevents re-definition warnings.

 * Fixed implicit function declaration warning concerning accept4.
   Defining `GNU_SOURCE` in the header file declares `accept4` when present
   (see man accept4).

0.6.2.0 Lars Petersen <info@lars-petersen.net> 2016-08-15

 * Added functions for constructing internet addresses without the need for IO
   (`inetAddressToTuple`, `inetAddressFromTuple`, `inet6AddressToTuple`,
    `inet6AddressFromTuple`) as proposed by Erik Rantapaa.

0.6.1.0 Lars Petersen <info@lars-petersen.net> 2016-08-11

 * A potential race condition has been fixed (issue #18): `c_get_last_error`
   was supposed to return the error code of the last operation (if any).
   On Linux et.al. it just returned `errno` whereas on Windows it wrapped
   a call to `WSAGetLastError`.
   The problem was that the value of `errno` and `WSAGetLastError` is only
   valid when sampled immediately after the failed call. This could not be
   easily guaranteed the way it was implemented: GHC's RTS is potentially
   allowed to interrupt the thread between the failed call and the call to
   `c_get_last_error` (although this is very unlikely when no memory allocation
   is necessary). The content of `errno` might have been reset of overridden
   by another thread.
   The solution for this is that all FFI calls now take a pointer with a reserved
   memory location (allocated on the stack, so it's quite cheap) and the C
   functions immediately save the errno (if necessary). The `unsafe ccall`s are
   guaranteed to be uninterruptible.

 * All tests have been ported to `tasty` as previously proposed by
   Roman Cheplyaka.

 * Fixed `connect` operation to use `getsockopt` with SO_ERROR to determine
   socket connection status / error code instead of issuing a second connection
   attempt (see issue #15).
   On Windows, the solution is a bit more difficult: `getsockopt` return 0
   unless the operation has either succeeded or failed.
   Unfortunately, there did not exist a mechanism to wait for this condition
   (GHC's IO manager lacks this feature). This has been circumvented by
   calling `select` for the socket with minimal timeout several times with
   an exponential back-off. Tests have been added to validate different aspects
   of this.

0.6.0.1 Lars Petersen <info@lars-petersen.net> 2016-04-10

 * Adapted the `AddrInfo` test suite to not depend on specific name resolution
   features that aren't available in a `chroot()` environment (see issue #12).

0.6.0.0 Lars Petersen <info@lars-petersen.net> 2016-03-26

 * Improved and adapted documentation.
 * Merged `GetSocketOption` and `SetSocketOption` to one single type class
   `SocketOption`.
 * `getNameInfo` now returns `NameInfo` instead of a tuple.
 * Added all theoretically possible `SocketExceptions`.
 * The type class `GetNameInfo` has been renamed to `HasNameInfo`.
 * The type class `GetAddressInfo` has been renamed to `HasAddressInfo`.
 * Removed operation `withConnectedSocket` without replacement.
   It should not be part of this minimal library. Its code can be retrieved from the repository if needed.
 * The operations `sendAll` and `receiveAll` are now exported through
   `System.Socket.Type.Stream` and no longer trough the main module.
   They are very specific, solely stream-oriented and just wrappers around
   the basic operations. Such operations shouldn't pollute the main module.
 * Issue #10: Ben Gamari reported that the associated type `SocketAddress`
   is not injective which would lead to compilation failure on GHC 8.* .
   This is fixed by using a data family instead.

0.5.3.0 Lars Petersen <info@lars-petersen.net> 2015-08-09

 * Added a test for `eOperationNotSupported` (try to listen on a UDP socket).
 * Niklas Hambüchen added `eOperationNotSupported`.

0.5.2.0 Lars Petersen <info@lars-petersen.net> 2015-07-08

 * Don't set `msgNoSignal` automatically with `send` and `sendTo`. This implicit
   behaviour is a bad design decision. The implications of this change are
   rather limited. The behaviour/correctness of an application is only affected
   if it hooked SIGPIPE. GHC's RTS by default ignores SIGPIPE since #1619.
   You're still advised to adapt your applications to use `msgNoSignal`
   explicitly when writing on stream oriented sockets. Otherwise the RTS gets
   unnecessarily interrupted. This is harmless, but annoying and not desired
   when developing high-performance applications.
 * Define `msgNoSignal` as 0 if not available and documented this behaviour.
 * Added new exception value `ePipe`.

0.5.1.0 Lars Petersen <info@lars-petersen.net> 2015-06-22

 * Exposed `unsafeGetSocketOption` and `unsafeSetSocketOption`.
 * Exposed `socketWaitRead` and `socketWaitWrite` through `System.Socket.Unsafe`.

0.5.0.0 Lars Petersen <info@lars-petersen.net> 2015-06-19

 * Introduced newtypes `Port`, `FlowInfo` and `ScopeId` in Inet6 family.
 * Renamed nearly everything in response to very constructive criticism
   by Bryan O'Sullivan. This is a breaking change (sorry about that).
   I felt this was the last chance to get this straight before the library
   gets widely adopted.
   Additional kudos to @ignorantone and @whatsthepoint.
 * Issue #7: Typo in documentation of inaddrNONE and inaddrBROADCAST.
   Kudos to Michael Fox.

0.4.0.1 Lars Petersen <info@lars-petersen.net> 2015-06-17

 * tar-ball did not contain relevant source files.

0.4.0.0 Lars Petersen <info@lars-petersen.net> 2015-06-16

 * Changed semantics of `connect` operation. It now
   blocks until a connection has either has been established or failed.
 * Added `SO_ERROR` socket option.
 * Added `eALREADY` exception constant.
 * Added `eISCONN` exception constant.
 * Added `eNOTCONN` exception constant.
 * Added convenience operation `withConnectedSocket`.
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
