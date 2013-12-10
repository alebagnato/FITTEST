#!/bin/bash

usage="./generate.sh [spinner | flex]  trace.csv outFolder [agglo]"

if [ $# -lt 3 ]
then
  echo $usage
  exit 1
fi

if [ -z "$BaseDir" ]
then
  BaseDir="/cs/research/crest/home0/lakhotia/stateAbstraction"
fi

DaikonDir=${BaseDir}/daikon
App="$1"
Log="$2"
Out="$3"
WekaFlag="-weka"

if [ $# -eq 4 -a "$4" == "agglo" ]
then
  WekaFlag=""
fi

#cleanup
if [ -e "nohup.out" ]
then
  rm nohup.out
fi

if [ -d "$Out" ]
then
  rm -rf "$Out"
fi

if [ -d "weka" ]
then
  rm -rf weka
fi

export PERL5LIB=${PERL5LIB}:${DaikonDir}/Text-CSV-1.21/lib

nohup java -Xmx3072m -jar ${BaseDir}/libs/stateAbstractionGA.jar -daikon ${DaikonDir} -t ${App} ${WekaFlag} ${Log} &

exit