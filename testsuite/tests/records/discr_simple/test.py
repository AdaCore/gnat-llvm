from subprocess import call

call(["llvm-gnatmake", "-q", "-gnatp", "test_discr.adb"])
call(["test_discr"])
