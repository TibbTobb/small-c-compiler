#!/bin/bash
input=$1
trimmed=${input::-2}

java -jar /home/toby/c_compiler/out/artifacts/c_compiler_jar/c_compiler.jar $input
gcc ${trimmed}.s -o  $trimmed