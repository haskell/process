//#OPTIONS: CPP

// #define JS_TRACE_PROCESS 1

#ifdef JS_TRACE_PROCESS
function h$logProcess() { h$log.apply(h$log,arguments); }
#define TRACE_PROCESS(args...) h$logProcess(args)
#else
#define TRACE_PROCESS(args...)
#endif

/*
   Convert from a string signal name to a signal number.

   To ensure consistent signal numbers between platforms we use signal
   numbers from the emscripten SDK whenever we use a numeric signal code.

   These might differ from the actual numbers of the operating system
   on which the nodejs process is running.

   list from emscripten /system/lib/libc/musl/arch/emscripten/bits/signal.h

   Note: we should possibly move this into the base or rts package in the future
  */
var h$process_signals = {
    'SIGHUP': 1,
    'SIGINT': 2,
    'SIGQUIT': 3,
    'SIGILL': 4,
    'SIGTRAP': 5,
    'SIGABRT': 6,
    'SIGIOT': 6,
    'SIGBUS': 7,
    'SIGFPE': 8,
    'SIGKILL': 9,
    'SIGUSR1': 10,
    'SIGSEGV': 11,
    'SIGUSR2': 12,
    'SIGPIPE': 13,
    'SIGALRM': 14,
    'SIGTERM': 15,
    'SIGSTKFLT': 16,
    'SIGCHLD': 17,
    'SIGCONT': 18,
    'SIGSTOP': 19,
    'SIGTSTP': 20,
    'SIGTTIN': 21,
    'SIGTTOU': 22,
    'SIGURG': 23,
    'SIGXCPU': 24,
    'SIGXFSZ': 25,
    'SIGVTALRM': 26,
    'SIGPROF': 27,
    'SIGWINCH': 28,
    'SIGIO': 29,
    'SIGPOLL': 29,
    'SIGPWR': 30,
    'SIGSYS': 31,
    'SIGUNUSED': 31
};

/*
   Create a one-directional pipe for communication with the child process

     - pipe: a Readable or Writable stream from spawning the child process
     - write: boolean - true if the pipe is for writing, false for reading
 */
function h$process_pipeFd(pipe, write) {
    var fdN = h$base_fdN--, fd = {};
    h$base_fds[fdN] = fd;
    TRACE_PROCESS("pipe", fdN, "opened, writable:", write);

    if(pipe && pipe._handle && typeof pipe._handle.fd === 'number') fd.fd = pipe._handle.fd;
    TRACE_PROCESS("pipe real fd", fd.fd);

    if(write) {
        fd.err   = null;
        fd.waiting = new h$Queue();
        fd.close = function(fd, fdo, c) { delete h$base_fds[fd]; pipe.end(); c(0); };
        fd.refs = 1;
        pipe.on('error', function(err) {
            TRACE_PROCESS("pipe received error", fd, err);
            fd.err = err;
        });
        fd.write = function(fd, fdo, buf, buf_offset, n, c)  {
            TRACE_PROCESS("pipe ", fd, " write:", n);
            if(fdo.err) {
                TRACE_PROCESS("pipe error", fdo.err);
                h$setErrno(fdo.err);
                c(-1);
                return;
            }
            var nbuf = buf.u8.slice(buf_offset, buf_offset+n);
            var r = pipe.write(nbuf, function() {
                TRACE_PROCESS("pipe", fd, "flushed");
                c(n);
            });
            TRACE_PROCESS("pipe write", fd, "result", r);
        }
    } else {
        fd.close      = function(fd, fdo, c) { delete h$base_fds[fd]; c(0); }
        fd.refs       = 1;
        fd.waiting    = new h$Queue();
        fd.chunk      = { buf: null, pos: 0, processing: false };
        fd.eof        = false;
        fd.err        = null;
        fd.reading    = false;

        pipe.on('end', function() {
            TRACE_PROCESS("pipe", fdN, fd.fd, "eof");
            fd.eof = true;
            h$process_process_pipe(fd, pipe);
        });
        pipe.on('error', function(err) {
            TRACE_PROCESS("pipe received error", fdN, fd.fd);
            fd.err = err;
            h$process_process_pipe(fd, pipe);
        });
        fd.read = function(fd, fdo, buf, buf_offset, n, c) {
            if(!fdo.reading) {
                /*
                   Reading is a blocking operation (asynchronous) from the Haskell
                   side. On the JavaScript side we rely on the 'readable' event to
                   know when there is available data. Every time data comes in we
                   process the queue of waiting read requests.

                   We don't attach the 'readable' event handler until we actually
                   read from the pipe, since the readable handler causes the node.js
                   process to start buffering data from the file descriptor.

                   If we don't read from the file descriptor it is unaffected by
                   node.js buffering and we can for example pass it to another child
                   process to allow direct communication between multiple child
                   processes.
                 */
                pipe.on('readable', function() {
                    TRACE_PROCESS("pipe", fdN, fd.fd, "readable");
                    h$process_process_pipe(fdo, pipe);
                });
                fdo.reading = true;
                h$process_process_pipe(fdo, pipe);
            }
            TRACE_PROCESS("pipe", fdN, fd.fd, "read", n, fdo.chunk.buf);
            fdo.waiting.enqueue({buf: buf, off: buf_offset, n: n, c: c});
            h$process_process_pipe(fdo, pipe);
        }
    }
    TRACE_PROCESS("created pipe, fd:", fdN);
    return fdN;
}

/*
    Process the queue of waiting read/write requests for a pipe
 */
function h$process_process_pipe(fd, pipe) {
    var c = fd.chunk;
    var q = fd.waiting;
    TRACE_PROCESS("processing pipe", fd);
    if(!q || !q.length() || c.processing) return;
    c.processing = true;
    while(fd.err && q.length()) {
        h$setErrno(fd.err);
        q.dequeue().c(-1);
    }
    if(!c.buf) {
        c.pos = 0;
        c.buf = pipe.read();
    }
    while(c.buf && q.length()) {
        var x = q.dequeue();
        var n = Math.min(c.buf.length - c.pos, x.n);
        for(var i=0;i<n;i++) {
            x.buf.u8[i+x.off] = c.buf[c.pos+i];
        }
        c.pos += n;
        x.c(n);
        // XXX this does reorder the requests if data comes in small chunks
        if(c.pos >= c.buf.length) c.buf = null;
        if(!c.buf && q.length()) {
            c.pos = 0;
            c.buf = pipe.read();
        }
    }
    while(fd.eof && q.length()) q.dequeue().c(0);
    TRACE_PROCESS("done processing pipe, remaining queue", q.length());
    c.processing = false;
}

/*
    Start an interactive child process using the node.js child_prcess.spawn
    functionality.

    Even though this is mostly a non-blocking operation (we don't wait until
    the child process has finished), this is an asynchronous function
    (with a continuation) because we want to wait until the child process has
    spawned before we resume the Haskell thread.

    This allows us to raise exceptions if the process could not be started, for
    example becaus of permission errors or a non-existent executable.

    Calls the continuation with a process object, or null when spawning the
    process has failed, after setting h$errno to the appropriate value.
 */
function h$process_runInteractiveProcess(
      cmd           // string  - command to run
    , args          // array of strings - arguments
    , workingDir    // string  - working directory, null: unchanged
    , env           // array of strings - environment [ key1, val1, key2, val2, ...]
                    //                         null: inherit
    , stdin_fd      // number   - stdin fd, -1: createpipe, -2: ignore
    , stdout_fd     // number   - stdout fd, -1: createpipe, -2: ignore
    , stderr_fd     // number   - stderr fd, -1: createpipe, -2: ignore
    , _closeFds     // boolean  - close file descriptors in child (ignored)
    , createGroup   // boolean  - create a new process group
    , delegateCtlC  // boolean  - delegate control-C handling
    , newSession    // boolean  - use posix setsid to start the process in a new session
    , childGID      // number   - child group id, -1 for unchanged
    , childUID      // number   - child user id, -1 for unchanged
    , c             // function - continuation, called when the process has spawned
    ) {
    TRACE_PROCESS("runInteractiveProcess");
    TRACE_PROCESS("cmd: ", cmd, " args: ", args);
    TRACE_PROCESS("workingDir: ", workingDir, " env: ", env);
    TRACE_PROCESS("stdin", stdin_fd, "stdout", stdout_fd, "stderr", stderr_fd);

    if(h$isNode()) {
        try {
            var stdin_p, stdout_p, stderr_p;

            function getStream(pos, spec) {
                // CreatePipe
                if(spec === -1) return 'pipe';

                // NoStream
                if(spec === -2) return 'ignore';

                // standard streams
                if(spec === 0) return spec == pos ? 'inherit' : process.stdin;
                if(spec === 1) return spec == pos ? 'inherit' : process.stdout;
                if(spec === 2) return spec == pos ? 'inherit' : process.stderr;

                // registered fd
                var stream = h$base_fds[spec];
                if(typeof stream.fd === 'number') return stream.fd;

                // raw fd
                if(typeof spec === 'number' && spec > 0) return spec;

                // unsupported stream type
                // the exception is caught and converted to an errno status code below
                throw new Error('EBADF');
            }

            stdin_p = getStream(0, stdin_fd);
            stdout_p = getStream(1, stdout_fd);
            stderr_p = getStream(2, stderr_fd);

            var options = { detached: newSession || createGroup
                          , stdio: [stdin_p, stdout_p, stderr_p]
                          };
            if(workingDir !== null) options.cwd = workingDir;
            if(env !== null) {
                var envObj = {};
                for(var i=0;i<env.length;i+=2) envObj[env[i]] = env[i+1];
                TRACE_PROCESS("environment: " + h$collectProps(envObj));
                options.env = envObj;
            }
            if(childGID !== -1) options.gid = childGID;
            if(childUID !== -1) options.uid = childUID;

            var procObj;
            var child;

            // node.js on Windows x86 sometimes throw an EBADF exception when
            // process.stdin is invalid, retry with ignored stdin when
            // this happens.
            TRACE_PROCESS("spawning process", cmd, args, options);

            try {
                child = h$child.spawn(cmd, args, options);
            } catch(e) {
                TRACE_PROCESS("spawning exception", e);

                if(e.toString().indexOf('EBADF') !== -1 && options.stdio[0] === process.stdin) {
                    options.stdio[0] = 'ignore';
                    child = h$child.spawn(cmd, args, options);
                } else {
                    throw e;
                }
            }
            TRACE_PROCESS("spawn done, setting handlers");

            // keep track of whether the process has spawned, since we can get
            // multiple events for this and we only want to call the continuation
            // once.
            var spawned = false;

            child.on('exit', function(code, sig) {
                TRACE_PROCESS("process finished", code, sig);
                // if the spawn event hasn't fired we still have to call
                // the continuation for that
                if(!spawned) {
                    spawned = true;
                    c(procObj);
                } else {
                    if(delegateCtlC) {
                        h$process_stopDelegateControlC();
                    }
                }
                if(code === null) {
                    // process was killed by a signal
                    var signo = h$process_signals[sig] || 31;
                    // The Haskell side expects a negative status if killed
                    // by a signal
                    code = -signo;
                }
                procObj.exit = code;

                // notify all threads that are waiting for the exit code
                for(var i=0;i<procObj.waiters.length;i++) {
                    procObj.waiters[i](code);
                }
            });

            child.on('spawn', function() {
                TRACE_PROCESS("process spawned");
                if(!spawned) {
                    if(delegateCtlC) {
                        h$process_startDelegateControlC();
                    }
                    spawned = true;
                    c(procObj);
                }
            });

            child.on('error', function(e) {
                TRACE_PROCESS("process errored:", e);
                if(!spawned) {
                    // if the process hasn't spawned yet we can raise an exception
                    // immediately

                    // prevent subsequent calls to the continuation
                    spawned = true;

                    h$setErrno(e);
                    c(null);
                } else {
                    /*
                       Convert the node.js status code string to the appropriate
                       (emscripten SDK) numeric error code.

                       We should add a way to get the error code from the
                       string without modifying the global h$errno value
                     */
                    var currentErrno = h$errno;
                    h$setErrno(e.code);
                    var code = h$errno;
                    h$errno = currentErrno;

                    procObj.exit = code;
                    for(var i=0;i<procObj.waiters.length;i++) {
                        procObj.waiters[i](code);
                    }
                }
            });

            procObj = {  fds: [ stdin_fd  === -1 ? h$process_pipeFd(child.stdio[0], true)  : null
                              , stdout_fd === -1 ? h$process_pipeFd(child.stdio[1], false) : null
                              , stderr_fd === -1 ? h$process_pipeFd(child.stdio[2], false) : null
                              ]
                        , exit: null
                        , waiters : []
                        , child: child
                    };
            TRACE_PROCESS("process object created:", procObj);

            // sometimes the process has already spawned before we attach the 'spawn` event handler,
            // check here if it already has a pid and immediately call the continuation if that's the
            // case
            if(typeof child.pid === 'number') {
                TRACE_PROCESS("process spawned immediately", child.pid);

                spawned = true;
                c(procObj);
            }

        } catch(e) {
            TRACE_PROCESS("catch:", e);
            spawned = true;
            h$setErrno(e);
            c(null);
        }
    } else { // h$isNode
        h$unsupported(null, c);
    }
}

/*
  return the thing to run as an array, first element the process,
  followed by the the args. null if no interpreter can be found

    - cmd: string - the command to run
    - args: array of string - arguments, null to run cmd as a shell command
              in an interpreter.
 */
function h$process_commandToProcess(cmd, args) {
    if(h$isNode()) {
        TRACE_PROCESS("commandToProcess: ", cmd, args);
        if(process.platform === 'win32') {
            if(args === null) { // shellcmd
                var com = process.env['COMSPEC'];
                /*
                   Note: The old GHCJS code had a fallback using code from
                         the directory package (h$directory_findExecutables).
                         Here we just produce an error if COMSPEC is not set,
                         since we don't have the code from the directory package
                         available.
                         If needed we could implement similar functionality here.
                  */
                if(!com) {
                    h$setErrno('ENOENT');
                    return null;
                }
                return [com, " /c " + cmd];
            } else {
                var r = [cmd];
                r = r.concat(args);
                return r;
            }
        } else {  // non-windows
            if(args === null) { // shellcmd
                return ["/bin/sh", "-c", cmd];
            } else {
                var r = [cmd];
                r = r.concat(args);
                return r;
            }
        }
    } else {
        return h$unsupported(null);
    }
}
/*
  Send the SIGTERM signal to the child process
 */
function h$process_terminateProcess(ph) {
    TRACE_PROCESS("terminateProcess", ph);
    if(h$isNode()) {
        ph.child.kill();
        return 0;
    } else {
        return h$unsupported(1);
    }
}

/*
   Get the process exit code. Does not block.

      - returns 0 if the process has not exited yet.
      - returns 1 and saves the exit code in the code_d pointer otherwise
 */
function h$process_getProcessExitCode(ph, code_d, code_o) {
    TRACE_PROCESS("getProcessExitCode", ph);
    if(ph.exit === null) return 0;
    code_d.i3[code_o>>2] = ph.exit;
    return 1;
}

/*
    Wait for the process to finish and return the exit code.
 */
function h$process_waitForProcess(ph, code_d, code_o, c) {
    TRACE_PROCESS("waitForProcess", ph);
    if(h$isNode()) {
        if(ph.exit !== null) {
            h$process_getProcessExitCode(ph, code_d, code_o);
            c(0);
        } else {
            ph.waiters.push(function(code) {
		        code_d.i3[code_o>>2] = code;
		        c(0);
	        });
        }
    } else {
        h$unsupported(-1, c);
    }
}

function h$process_interruptProcessGroupOf(ph) {
    TRACE_PROCESS("interruptProcessGroupOf", ph);
    if(h$isNode()) {
        // there doesn't appear to be a way to find the process
        // group id from a process id (ph.child.pid) on nodejs,
        // so this operation is unsupported.
        return h$unsupported(-1);
    } else {
        return h$unsupported(-1);
    }
}

var h$process_delegateControlCCount = 0;

/*
  We install a signal handler that ignores SIGINT/SIGQUIT while
  delegating ctl-c handling.

  This keeps the current node.js process running and propagates the
  signal to the child processes in the same group.
 */
function h$process_ignoreSIG() {
    TRACE_PROCESS("process_ignoreSIG: ignoring signal");
    return 0;
}

/*
   Start delegating ctl-c handling. Installs the above handler if this is the
   first process for which delegation is needed.
 */
function h$process_startDelegateControlC() {
    TRACE_PROCESS("startDelegateControlC", h$process_delegateControlCCount);
    if(h$isNode()) {
        if(h$process_delegateControlCCount === 0) {
            TRACE_PROCESS("startDelegateControlC: installing handler")
            process.on('SIGINT', h$process_ignoreSIG);
            process.on('SIGQUIT', h$process_ignoreSIG);

        }
        h$process_delegateControlCCount++;
        return 0;
    } else {
        return h$unsupported(-1);
    }
}

/*
   Stop delegating ctrl-c handling. Removes the above handler if this is the
   last process for which delegation is needed.
 */
function h$process_stopDelegateControlC() {
    TRACE_PROCESS("stopDelegateControlC", h$process_delegateControlCCount);
    if(h$isNode()) {
        if(h$process_delegateControlCCount > 0) {
            h$process_delegateControlCCount--;
            if(h$process_delegateControlCCount === 0) {
                TRACE_PROCESS("stopDelegateControlC: removing handler")
                process.off('SIGINT', h$process_ignoreSIG);
                process.off('SIGQUIT', h$process_ignoreSIG);
            }
        }
        return 0;
    } else {
        return h$unsupported(-1);
    }
}

/*
   Get the process id of the current (node.js) process
 */
function h$process_getCurrentProcessId() {
    TRACE_PROCESS("getCurrentProcessId");
    if(h$isNode()) {
        return process.pid;
    } else {
        return h$unsupported(-1);
    }
}

/*
   Get the process id of a child process
 */
function h$process_getProcessId(ph) {
    TRACE_PROCESS("getProcessId", ph);
    if(ph && typeof ph === 'object' &&
       ph.child && typeof ph.child == 'object' &&
       typeof ph.child.pid == 'number') {
        return ph.child.pid;
    } else {
        h$setErrno('EBADF');
        return -1;
    }
}
