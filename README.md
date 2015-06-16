socket
======

[![Available on Hackage][badge-hackage]][hackage]
[![License MIT][badge-license]][license]
[![Build Status][badge-travis]][travis]

### Motivation

This library aims to expose a minimal and platform-independant interface for
POSIX compliant networking code.

### Implementation Philosophy

  - Every operation and every flag exposed should be supported with same
    semantics on every platform. If this cannot be guaranteed it should
    be supplied by another (extension) package.
    Examples for things that have been ripped out of this library are:
      - Support for Unix sockets which don't have an equivalent on Windows.
      - Support for SCTP.
      - Support for vectored IO (at least unless it can be guaranteed to
        be supported on all platforms).

  - Absolutely no conditional exports.

  - No `#ifdef` madness in the Haskell sources. The Haskell binding code
    uses the FFI to reference the platform's native networking functions.
    If they are not Posix compliant (i.e. under Windows) an level of
    indirection is introduced to write an Posix compliant equivalent in C
    using whatever the plaform specific building blocks are.

### Platform Support

#### Linux

Working.

#### BSD

Unknown. Should work. Please report if not.

#### MacOS

Unknown. Please report if you have a Mac.

#### Windows

Fully supported on Windows7 (maybe Vista) or higher :-)

GHCs runtime system on Windows does not offer an event notification mechanism for sockets.
The original [network](https://hackage.haskell.org/package/network) library
suffers from this, too. For example, connection attempts are uninterruptible etc.
The approach taken to circumvent this in this library is to poll the
non-blocking sockets with increasing delay. This guarantees interruptability
and fairness between different threads. It allows for decent throughput
while also keeping CPU consumption on a moderate level if a socket has not seen
events for a longer period of time (maximum of 1 second delay after 20
polling iterations). The only drawback is potentially reduced response time
of your application. The good part: Heavy load (e.g. connection requests or
incoming traffic) will reduce this problem. Eventually your accepting thread
won't wait at all if there are several connection requests queued.

This workaround may be removed if someone is willing to sacrifice to improve
the IO manager on Windows.

### Dependencies

   - base
   - bytestring

### Tests

Run the default test suites:

```bash
cabal test
```

[badge-travis]: https://img.shields.io/travis/lpeterse/haskell-socket.svg
[travis]: https://travis-ci.org/lpeterse/haskell-socket
[badge-hackage]: https://img.shields.io/hackage/v/socket.svg?dummy
[hackage]: https://hackage.haskell.org/package/socket
[badge-license]: https://img.shields.io/badge/license-MIT-green.svg?dummy
[license]: https://github.com/lpeterse/haskell-socket/blob/master/LICENSE
[issues]: https://github.com/lpeterse/haskell-socket/issues
[Github]: https://github.com/lpeterse/haskell-socket