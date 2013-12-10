echo %time%
java -cp ../classes;. ocon.entry.OptimizeKingProperties -contest ../../../../../IBM/JavaConTest/Lib/ConTest.jar -classesToInstr ocon.test.datarace -testSuiteClasses ocon.tests.datarace.SimpleDataRaceTest -cls . -output results
echo %time%
