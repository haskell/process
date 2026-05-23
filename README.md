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

This sdist ought to be generated as follows:
- navigate to [the "prepare release artifacts"
  workflow](https://github.com/haskell/process/actions/workflows/prepare-release-artifacts.yml)
- click through to the workflow run you want to create an
  sdist for. Or, if none was created automatically then launch
  a run using the "`workflow_dispatch` event trigger" that you will find there.
- carefully sanity check the output of the "smoketest" jobs in github UI.
  These can also be downloaded as an artifact for local inspection. A sanity check
  would e.g. entail look at whether `posix_spawn_file_actions_addchdir_np` is present
- afterwards, download the sdist from the release page by clicking on "Summary",
  scrolling to the bottom of the page,
  then downloading the `sdist`. Upload it as a candidate and proceed as usual.
