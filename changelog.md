# Changelog for [`process` package](http://hackage.haskell.org/package/process)

## 1.2.3.0 *March 2015*

  * [Meaningful error message when exe not found on close\_fds is
  True](https://ghc.haskell.org/trac/ghc/ticket/3649#comment:10)

  * New functions `readCreateProcess` and `readCreateProcessWithExitCode`

## 1.2.2.0  *Jan 2015*

  * Fix delegated CTRL-C handling in `createProcess` in case of failed
    process creation. See [issue #15](https://github.com/haskell/process/issues/15)
    for more details.

  * `waitpid` on child PID after pre-exec failure in child to prevent zombies.
    See also [issue #14](https://github.com/haskell/process/issues/14).

## 1.2.1.0  *Dec 2014*

  * Add support for `base-4.8.0.0`

  * Remove Hugs98 specific code

  * New `IsString CmdSpec` instance

  * Expose documentation for `System.Process.Internals`

  * With GHC 7.10, `System.Cmd` and `System.Process` are now `Safe`
    (when compiled with older GHC versions they are just `Trustworthy`)

  * Expose `createProcess_` function, and document behavior of `UseHandle` for
    `createProcess`. See [issue #2](https://github.com/haskell/process/issues/2).

  * New `System.Process.createPipe` operation.
    See also [GHC #8943](https://ghc.haskell.org/trac/ghc/ticket/8943)

## 1.2.0.0  *Dec 2013*

  * Update to Cabal 1.10 format
  * Remove NHC specific code
  * Add support for `base-4.7.0.0`
  * Improve `showCommandForUser` to reduce redundant quoting
  * New functions `callProcess`, `callCommand`, `spawnProcess` and `spawnCommand`
  * Implement WCE handling according to http://www.cons.org/cracauer/sigint.html
  * New `delegate_ctlc` field in `CreateProcess` for WCE handling
  * Use `ExitFailure (-signum)` on Unix when a proc is terminated due to
    a signal.
  * Deprecate `module System.Cmd`
  * On non-Windows, the child thread now comunicates any errors back
    to the parent thread via pipes.
  * Fix deadlocks in `readProcess` and `readProcessWithExitCode`
