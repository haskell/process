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

To make the release process more observable and less dependent on the system
of the maintainer, there's a workflow creating an sdist.

This sdist ought to be used as follows:
- create a workflow run for the respective commit
- navigate to the "prepare release artifacts" job group in the github UI
- carefully sanity check the output of the sanity checks ("smoketest") in github UI.
  These can also be downloaded as an artifact for local inspection. A sanity check
  would e.g. entail look at whether `poxis_spawn.*` symbols are recognised and
  configured correctly.
- afterwards, download the sdist from the release page by scrolling all the way down, 
  then downloading the `sdist`. Upload it as a candidate and proceed as usual.
