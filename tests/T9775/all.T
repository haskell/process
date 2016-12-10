
test('T12725_fail',
     [extra_clean(['ok.o', 'ok.exe', 'main.o', 'main.exe']),
     [unless(opsys('mingw32'),skip)]
      pre_cmd('$MAKE -s --no-print-directory T12725')],
     compile_and_run, [''])

test('T12725_good',
     [extra_clean(['ok.o', 'ok.exe', 'main.o', 'main.exe']),
     [unless(opsys('mingw32'),skip)]
      pre_cmd('$MAKE -s --no-print-directory T12725')],
     compile_and_run, [''])
