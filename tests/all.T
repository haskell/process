test('process001', extra_clean(['process001.out']), compile_and_run, [''])
test('process002', extra_clean(['process002.out']), compile_and_run, [''])
test('process003', omit_ways(['ghci']), compile_and_run, [''])

if config.platform == "i386-unknown-mingw32":
      proc004_config = exit_code(1)
else:
      proc004_config = normal

test('process004', [ proc004_config, normalise_exe ], compile_and_run, [''])
test('1780', normal, compile_and_run, [''])
test('process005', normal, compile_and_run, [''])
test('process006', normal, compile_and_run, [''])

test('process007', [extra_clean(['process007.tmp']), reqlib('unix')], compile_and_run, [''])
test('process008', normal, compile_and_run, [''])

# not the normal way: this test runs processes from multiple threads, and
# will get stuck without the threaded RTS.
test('3231',
     [only_ways(['threaded1','threaded2']),
      extra_clean(['foo1.txt', 'foo2.txt'])],
     compile_and_run,
     [''])
test('4198',
     [cmd_prefix('\'' + config.compiler + '\'' + ' exitminus1.c -no-hs-main -o exitminus1; '),
      extra_clean(['exitminus1.o', 'exitminus1', 'exitminus1.exe'])],
     compile_and_run,
     [''])

test('3994', only_ways(['threaded1','threaded2']), compile_and_run, [''])
