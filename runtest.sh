#! /bin/sh

cd llvm-backend 
make bin
cd ..
PATH=$PWD/llvm-backend/bin:$PATH llvm-backend/bin/llvm-gnatcompile -c tests/$1
