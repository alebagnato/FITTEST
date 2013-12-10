#!/bin/bash

# Make sure the asic compiler has been build.
# > cabal install  in lib directory
# > cabal install  in compiler directory
# Also make sure that the test application is compiled to a .swf file
# > ~/Work/flex_sdk_4/bin/mxmlc Demo.mxml
# Adapt the path below to that file:
TESTFILE=Demo.swf


# Path to the compiler in the build directory
ASIC=../compiler/dist/build/asic/asic

# cleanup
echo "cleanup"
rm -f *.o
rm -f *.hi

# pretty print AST to text file to allow inspection
echo "pretty print"
$ASIC --dump-ast $TESTFILE --verbose > demo-ast.txt

# generate Haskell support library
echo "generate haskell"
$ASIC --env=../lib/src/InstrBaseLib.hs --gen-lib=DemoSym $TESTFILE --verbose -o DemoSym.hs
ghc -c -O DemoSym.hs -i../lib/src

# compile instrumentation program
echo "compile haskell"
ghc -c -O DemoInstr.hs -i../lib/src
ghc --make -O DemoInstr.hs -i../lib/src -o demo

# run instrumentation program
echo "generating core code"
./demo > demo-core.txt
