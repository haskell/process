The `process` Package  [![Hackage](https://img.shields.io/hackage/v/process.svg)](https://hackage.haskell.org/package/process) ![Tests](https://github.com/haskell/process/workflows/Tests/badge.svg)
=====================

See [`process` on Hackage](http://hackage.haskell.org/package/process) for
more information.

Installing from Git
-------------------

To build this package using Cabal directly from Git, you must run
`autoreconf -i` before the usual Cabal build steps (`cabal
{configure,build,install}`). The program `autoreconf` is part of
[GNU autoconf](http://www.gnu.org/software/autoconf/).  There is no
need to run the `configure` script: `cabal configure` will do this for
you.

## Maintainers

Some generated files not tracked in the git repository are included in
a source (`sdist`) distribution, including
`include/HsProcessConfig.h.in` and `configure`.  Maintainers who are
producing a release of `process`, for example to upload to Hackage,
should ensure that they run the following to generate fresh files:

```
git clean -fxd
autoconf -i
```

(WARNING: `git clean -fxd` will delete all untracked files in the
repo)
