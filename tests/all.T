# some platforms use spawnp instead of exec in some cases, resulting
# in spurious error output changes.
normalise_exec = normalise_fun(lambda s: s.replace('spawnp', 'exec'))

test('process001', extra_clean(['process001.out']), compile_and_run, [''])
test('process002', [fragile_for(16547, concurrent_ways), extra_clean(['process002.out'])], compile_and_run, [''])
test('process003', [fragile_for(17245, concurrent_ways), omit_ways(['ghci'])], compile_and_run, [''])
test('process004', [normalise_exec, normalise_exe], compile_and_run, [''])
test('T1780', normal, compile_and_run, [''])
test('process005', omit_ways(['ghci']), compile_and_run, [''])
test('process006', normal, compile_and_run, [''])

test('process007',
     [extra_clean(['process007.tmp',
                   'process007_fd.o', 'process007_fd', 'process007_fd.exe']),
      reqlib('unix'),
      pre_cmd('$MAKE -s --no-print-directory process007_fd')],
     compile_and_run, [''])
test('process008', normal, compile_and_run, [''])

# not the normal way: this test runs processes from multiple threads, and
# will get stuck without the threaded RTS.
test('T3231',
     [only_ways(['threaded1','threaded2']),
      extra_clean(['foo1.txt', 'foo2.txt'])],
     compile_and_run,
     [''])
test('T4198',
     [pre_cmd('{compiler} exitminus1.c -no-hs-main -o exitminus1'),
      extra_clean(['exitminus1.o', 'exitminus1', 'exitminus1.exe'])],
     compile_and_run,
     [''])

test('T3994', [only_ways(['threaded1','threaded2']),
               extra_files(['T3994app.hs']),
               pre_cmd('$MAKE -s --no-print-directory T3994app')],
     compile_and_run, [''])
test('T4889', normal, compile_and_run, [''])

test('process009', when(opsys('mingw32'), skip), compile_and_run, [''])
test('process010', normalise_exec, compile_and_run, [''])
test('process011', when(opsys('mingw32'), skip), compile_and_run, [''])

test('T8343', normal, compile_and_run, [''])
