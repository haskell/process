# Changelog for [`process` package](http://hackage.haskell.org/package/process)

## 1.6.19.0 *April 2024*

* Adjust command-line escaping logic on Windows to ensure that occurrences of
  characters with special significance to the Windows batch interpreter are
  properly escaped in arguments passed to `.bat` and `.cmd` processes.
  This addresses
  [HSEC-2024-0003](https://github.com/haskell/security-advisories/tree/main/advisories/hackage/process/HSEC-2024-0003.md).

## 1.6.18.0 *September 2023*

* Fix deadlock when waiting for process completion and process jobs [#273](https://github.com/haskell/process/issues/273)
* Support `delegate_ctlc` on Windows. [#278](https://github.com/haskell/process/pull/278)
* Drop support for `vfork` [#261](https://github.com/haskell/process/pull/261)
* Javascript backend support
* Fix potential segmentation fault on macOS [#295](https://github.com/haskell/process/pull/295)

## 1.6.17.0 *February 2023*

* Improved documentation for the `OpenExtHandle` constructor.

## 1.6.16.0 *October 2022*

* `posix_spawn`: Don't rely on addclose not failing for closed fds [#251](https://github.com/haskell/process/issues/251)
* Support unix 2.8 [#258](https://github.com/haskell/process/issues/258)

## 1.6.15.0 *August 2022*

* Correct permissions on createPipe on Windows [234](https://github.com/haskell/process/pull/234)
* Ensure that both ends of pipes on Windows are created in the same mode  [234](https://github.com/haskell/process/pull/234)
* Fixed an issue with WINIO where giving an application an inherited pipe can cause it to misbehave [245](https://github.com/haskell/process/pull/245)
* Set the encoding on WINIO created pipes to the local encoding as with MIO [248](https://github.com/haskell/process/pull/248)
* cbits/fork-exec: Don't dup2 identical fds [#250](https://github.com/haskell/process/pull/250)

## 1.6.14.0 *February 2022*

* posix: Ensure that `errno` is set after `posix_spawnp` fails [#228](https://github.com/haskell/process/pull/228)
* Fix `waitForProcess` not closing process handles with `delegate_ctlc` [#231](https://github.com/haskell/process/pull/231)
* Don't use `posix_spawn` on platforms where it does not report `ENOENT` in caes where the
  requested executable does not exist [#224](https://github.com/haskell/process/issues/224)
* Ensure that `find_executable` correctly-locates executables when a change in
  working directory is requested [#219](https://github.com/haskell/process/issues/219)
* Fix capitalization error allowing `execvpe` to be used when available.

## 1.6.13.2 *July 2021*

* `posix_spawn`: Don't attempt to `dup2` identical fds [#214](https://github.com/haskell/process/pull/214)

## 1.6.13.1 *July 2021*

* Patches for the previous release

## 1.6.13.0 *July 2021*

* Refactoring of POSIX process logic [#208](https://github.com/haskell/process/pull/208)

## 1.6.12.0 *June 2021*

* Add function `getCurrentPid` to get the currently executing process' ID [#205](https://github.com/haskell/process/pull/205)

## 1.6.11.0 *January 2021*

* Windows: Add support for new I/O manager in GHC 8.12[#177](https://github.com/haskell/process/pull/177)
* Deprecate use of `createPipeFd` in favor of `createPipe`
* Fix MVar re-entrant problem on Windows with `terminateProcess` and process jobs. See [#199](https://github.com/haskell/process/pull/199)

## 1.6.10.0 *June 2020*

* Give a usable buffer to `_pipe` on Windows [#182](https://github.com/haskell/process/pull/182)

## 1.6.9 *May 2020*

* Windows: Fix buffer size of `QueryInformationJobObject` request [#176](https://github.com/haskell/process/pull/176/files)

## 1.6.8.2 *March 2020*

* Fix another process wait bug on Windows.

## 1.6.8.1 *March 2020*

* Fix a few warnings on Windows.

## 1.6.8.0 *February 2020*

* Fix several bugs on Windows where use of process jobs would result
  in the process being prematurely terminated. See
  [#168](https://github.com/haskell/process/168).

## 1.6.7.0 *November 2019*

* Fix a race condition on Windows that happens when you use process jobs and one of
  the child processes terminates but doesn't release its resources immediately.
  Control returns to the caller too soon in this scenario. See [#159](https://github.com/haskell/process/pull/159)

## 1.6.6.0 *October 2019*

* Fix a potential privilege escalation issue (or, more precisely, privileges
  not being dropped when this was the user's intent) where the groups of the
  spawning process's user would be incorrectly retained due to a missing call to
  `initgroups` [#149].
* Bug fix: Prevent stripping undecodable bytes from environment variables
  when in a non-unicode locale.
  [#152](https://github.com/haskell/process/issues/152)
* Expose `runInteractiveProcess_lock` in `System.Process.Internals`
  [#154](https://github.com/haskell/process/pull/154)

## 1.6.5.1 *June 2019*

* Version bound bumps
* Slightly nicer error messages for internal errors

## 1.6.5.0 *December 2018*

* Bug fix: On Windows ignore ERROR_ACCESS_DENIED for TerminateProcess() if the process did terminate
  [#110](https://github.com/haskell/process/issues/110)
* Improve documentation of the `NoStream` data constructor

## 1.6.4.0 *July 2018*

* Bug fix: Don't leak pipes on failure
  [#122](https://github.com/haskell/process/issues/122)
* Expose `cleanupProcess` from `System.Process`
  [#130](https://github.com/haskell/process/pull/130)
* Drop support for GHC before 7.10.3

## 1.6.3.0 *January 2018*

* Added `getPid` and export of platform specific `Pid` type
  [#109](https://github.com/haskell/process/pull/109)

## 1.6.2.0 *October 2017*

* Allow async exceptions to be delivered to masked thread calling `waitForProcess`
  [#101](https://github.com/haskell/process/pull/101)
* Update Win32 package version to 2.6.x

## 1.6.1.0 *July 2017*

* Expose `CGid`, `GroupID`, and `UserID` from `System.Process.Internals`
  [#90](https://github.com/haskell/process/issues/90)
  [#91](https://github.com/haskell/process/pull/91)

## 1.6.0.0 *February 2017*

* Fix: waitForProcess race condition
  [#46](https://github.com/haskell/process/issues/46)
  [#58](https://github.com/haskell/process/pull/58)

## 1.5.0.0 *February 2017*

* Bug fix: Don't close already closed pipes
  [#81](https://github.com/haskell/process/pull/81)
* Relax version bounds of Win32 to allow 2.5.
* Add support for monitoring process tree for termination with the parameter `use_process_jobs`
  in `CreateProcess` on Windows. Also added a function `terminateJob` to kill entire process tree.

## 1.4.3.0 *December 2016*

* New exposed `withCreateProcess`
* Derive `Show` and `Eq` for `CreateProcess`, `CmdSpec`, and `StdStream`

## 1.4.2.0 *January 2016*

* Added `createPipeFD` [#52](https://github.com/haskell/process/pull/52)
    * New function `createPipeFD` added which returns a POSIX File Descriptor (CInt)
      instead of a GHC Handle to a pipe

## 1.4.1.0 *November 2015*

* Use less CPP [#47](https://github.com/haskell/process/pull/47)
    * Refactor to have separate Windows and POSIX modules internally
    * Remove the broken non-GHC code paths

## 1.4.0.0 *November 2015*

* Added `child_user` and `child_group` to `CreateProcess` for unix. [#45](https://github.com/haskell/process/pull/45)

## 1.3.0.0 *August 2015*

* Add `StdStream(NoStream)` to have standard handles closed. [#13](https://github.com/haskell/process/pull/13)
* Support for Windows `DETACHED_PROCESS` and `setsid` [#32](https://github.com/haskell/process/issues/32)
* Support for Windows `CREATE_NEW_CONSOLE` [#38](https://github.com/haskell/process/issues/38)

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
