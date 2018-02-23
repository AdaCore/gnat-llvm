from subprocess import call

call(["llvm-gnatmake", "-q", "concat.adb"])
call(["./concat"])
