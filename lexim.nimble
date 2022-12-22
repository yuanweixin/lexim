
version = "1.0"
license = "MIT"

srcDir        = "src"
installExt    = @["nim"]

requires "nim >= 1.6.8"
requires "fusion"

import ospaths

proc buildHelper(name: string) =
  if not fileExists(name.toExe):
    exec "nim c " & name

task make, "builds Lexim and an example":
  buildHelper "lexe"
  exec "nim c ex1"

task tests, "test regular expressions":
  exec "nim c -r tests"
