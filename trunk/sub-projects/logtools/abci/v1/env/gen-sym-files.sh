#!/bin/sh

##
##

# FLEX="/Users/ariem/Work/flex/frameworks/libs"
FLEX="/Users/ariem/Work/flex/frameworks/locale/en_US"
COMP="../compiler/dist/build/asic/asic"
FILES=`find $FLEX | grep swc`

for f in $FILES
do
  OUTP=`basename $f ".swc"`
  echo "running: $COMP --export-sym=sdk-4.5/$OUTP.env $f"
  $COMP --export-sym=sdk-4.5/$OUTP.env $f +RTS -K256M -RTS
done
