test('process001', extra_clean(['process001.out']), compile_and_run, [''])
test('process002', extra_clean(['process002.out']), compile_and_run, [''])
test('process003', omit_ways(['ghci']), compile_and_run, [''])

if config.platform == "i386-unknown-mingw32":
      proc004_config = exit_code(1)
else:
      proc004_config = normal

test('process004', proc004_config, compile_and_run, [''])
test('1780', normal, compile_and_run, [''])
test('process005', normal, compile_and_run, [''])
test('process006', normal, compile_and_run, [''])

test('process007', [extra_clean(['process007.tmp']), reqlib('unix')], compile_and_run, [''])
test('process008', normal, compile_and_run, [''])

# not the normal way: this test runs processes from multiple threads, and
# will get stuck without the threaded RTS.
test('3231', only_ways(['threaded1','threaded2']), compile_and_run, [''])
