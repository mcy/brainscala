# brainscala
Brainfuck in Scala, with infinite memory tape!

Build with `sbt assembly`

## Main Options

* `-c` turns the input into a literal string
* `-v` prints verbose output (which there is none of yet)
* `-e` outputs the last value on the pointer as the exit code.

## Engines

* `interpret` interprets the script without compilation to VM or native code
* `gcc` compiles the script to C, which is then compiled to native code and output as an executable
* `jni` produces a complete JNI binding to a gcc'ed script, complete with template class