test('process001', normal, compile_and_run, [''])
clean(['output'])
test('process002', normal, compile_and_run, [''])
clean(['output'])
test('process003', omit_ways(['ghci']), compile_and_run, [''])

if config.platform == "i386-unknown-mingw32":
      proc004_config = exit_code(1)
else:
      proc004_config = normal

test('process004', proc004_config, compile_and_run, [''])
test('1780', normal, compile_and_run, [''])
test('process005', normal, compile_and_run, [''])
test('process006', normal, compile_and_run, [''])

test('process007', reqlib('unix'), compile_and_run, [''])
test('process008', normal, compile_and_run, [''])
