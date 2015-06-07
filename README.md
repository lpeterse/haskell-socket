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

Unfinished (problem with non-blocking IO).

Aim: Support Windows7 or higher. Don't have dependencies on autotools, just
Haskell Platform with MinGW should suffice.

#### Android

Unknown. Should be supported. Please get in touch if you plan to use it.

### Dependencies

   - base
   - bytestring

### Tests

Run the default test suite:

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