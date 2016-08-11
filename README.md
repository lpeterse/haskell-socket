socket
======

[![Available on Hackage][badge-hackage]][hackage]
[![License MIT][badge-license]][license]
[![Build Status][badge-travis]][travis]

### Motivation

This library aims to expose a minimal and cross platform interface for
POSIX compliant networking code.

### Implementation Philosophy

  - Every operation and every flag exposed should be supported with same
    semantics on every platform. If this cannot be guaranteed it should
    be supplied by another (extension) package.

  - Absolutely no conditional exports.

  - No `#ifdef` madness in the Haskell sources. The Haskell binding code
    uses the FFI to reference platform dependant C functions for each operation.
    If a platform is not POSIX compliant (i.e. Windows) equivalent functionality
    is implemented by using whatever the platform specific building blocks are.

### Platform Support

#### Linux

Platform is fully supported. Each commit and release is automatically tested with
[Travis CI](https://travis-ci.org/lpeterse/haskell-socket) and several versions
of GHC.

#### Windows

Fully supported on Windows7 (maybe Vista) or higher :-)

GHC's runtime system on Windows does not offer an event notification mechanism for sockets.
The original [network](https://hackage.haskell.org/package/network) library
suffers from this, too. For example, connection attempts are non-interruptible etc.
The approach taken to circumvent this in this library is to poll the
non-blocking sockets with increasing delay. This guarantees non-interruptability
and fairness between different threads. It allows for decent throughput
while also keeping CPU consumption on a moderate level if a socket has not seen
events for a longer period of time (maximum of 1 second delay after 20
polling iterations). The only drawback is potentially reduced response time
of your application. The good part: Heavy load (e.g. connection requests or
incoming traffic) will reduce this problem. Eventually your accepting thread
won't wait at all if there are several connection requests queued.

This workaround may be removed if someone is willing to sacrifice to improve
the IO manager on Windows.

Each release is manually tested on a Windows 10 virtual machine with the
latest Haskell Platform (64bit).

#### MacOS

Working, but not regularly tested.

Please report when it is no longer working on MacOS.

### Dependencies

   - base
   - bytestring

### Tests

The project uses [tasty](http://documentup.com/feuerbach/tasty) for testing.

There are two test suites: `default` and `threaded` which are using the same
code. Only difference is that one is compiled against GHC's single threaded RTS
and the other against the multi-threaded one. Run `cabal test` to run both
in sequence.

In order to see details and colored output you may also want to try

```bash
ghc --make test/test.hs && ./test/test
```

[badge-travis]: https://img.shields.io/travis/lpeterse/haskell-socket.svg
[travis]: https://travis-ci.org/lpeterse/haskell-socket
[badge-hackage]: https://img.shields.io/hackage/v/socket.svg?dummy
[hackage]: https://hackage.haskell.org/package/socket
[badge-license]: https://img.shields.io/badge/license-MIT-green.svg?dummy
[license]: https://github.com/lpeterse/haskell-socket/blob/master/LICENSE
[issues]: https://github.com/lpeterse/haskell-socket/issues
[Github]: https://github.com/lpeterse/haskell-socket
