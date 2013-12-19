#!/bin/bash

echo "Generating FSM Monitor ..."
java -jar ../../efsm2selenium-1.0.1.jar -d java -p "Test" --java-tmpl  eu/fittest/fbk/efsm2ct/efsm2mon/vm/fsm-int.vm  Cart.efsm test

echo "Compiling sources ..."
javac -classpath ../../efsm2selenium-1.0.1.jar:java java/*/*.java

echo "Generating Test suite ..."
java -jar ../../tools/evosuite/evosuite-minimal.jar -generateSuite -class test.TestCart -cp ../../efsm2selenium-1.0.1.jar:java  -Dshow_progress=true -Dsearch_budget=30 -Dstopping_condition=MaxTime
