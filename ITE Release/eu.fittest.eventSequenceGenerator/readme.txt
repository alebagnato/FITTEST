eventSequenceExtractor extracts the sequences of event from FSMs, i..e, the abstract test cases.

It can be executed in the following ways:

----------------------------- (1) from command line 

The main class is: eu.fittest.eventSequenceGenerator.MainToImport.
It contains the main method that can be executed as follows:

--(1.a) using the full list of parameters (i.e., 12 parameters)

/**
String usage = "usage: java -jar EventSequenceGenerator inputFolder inputFolderTracess outFolder abstractTCName criterion k typeOfVisit typeOfFitness N_max t_initial SuiteIt_max TcIt_max\n" 
			 + "   where: \n"			
	    	 + "   inputFolder \t: path to the folder of the actual fsm (e.g., default: 'input/tudu_actual/fsm.fsm')\n"
	    	 + "   inputFolderTraces \t: path to the folder of the traces (e.g., default: 'input/logs')\n"
	        + "    outFolder \t: path to the folder of the generated traces (e.g., default: 'generatedPaths'); \n"
	        + "    abstractTCName \t: (prefix)name for the generated abstrac test cases (e.g., default: 'abstractTC'); \n"
	        + "    criterion \t: the criterion used to extract event sequences (e.g., default: 'VISITOR_COVERAGE_UNIFORM' - VISITOR_BREADTHFIRST,VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS,VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS, VISITOR_BREADTHFIRST_2,VISITOR_BREADTHFIRST_2,VISITOR_COVERAGEWITHFRQ_LOGS, VISITOR_xLeastFrequentEven_LOGS, VISITOR_COVERAGE_UNIFORM_LOGS, VISITOR_COVERAGE_UNIFORM,VISITOR_SEQ_MAXK,VISITOR_SEQK,VISITOR_SEMK,VISITOR_SEM_MAXK,VISITOR_SEM_LastEventPair_MAXK, VISITOR_SEM_LastEventPair_K,VISITOR_ALT_MAXK,VISITOR_DIVERSITY_TC,VISITOR_DIVERSITY_TL,VISITOR_DIVERSITY_EDM,VISITOR_DIVERSITY_EDA,VISITOR_DIVERSITY_LastEventPair_TC, VISITOR_DIVERSITY_LastEventPair_TL,VISITOR_DIVERSITY_LastEventPair_EDA,VISITOR_DIVERSITY_LastEventPair_EDM); \n"
	        + "    k \t: the k parameter - only for criterion={all_sequences_maxK,all_sequences_K,sem_maxK,sem_K,alt_maxK,alt_K} (e.g., default: '2'); \n"
	        + "    typeOfVisit \t: the typeOfVisit for paths/sequences extraction - only for criterion={all_sequences_maxK,all_sequences_K,sem_maxK,sem_K,alt_maxK,alt_K} (e.g., default: 'BreadthFirstVisit' - BreadthFirstVisit,BreadthFirstVisitWithLoop,BreadthFirstVisitWithGlobalLoop,givenLengthK); \n"
	        + "    typeOfFitness \t: the typeOfFitness for paths extraction - only for criterion={diversity} (e.g., default: 'TC_absolute' - TC_absolute, TL, EDM, EDA); \n"
	        + "    N_max \t: the max paths_suite size - only for criterion={diversity} (e.g., default: '100' ); \n"
	        + "    t_initial \t: initial temperature value - only for criterion={diversity} (e.g., default: '5' ); \n"
	        + "    SuiteIt_max \t: number of paths considered in the evolution - only for criterion={diversity} (e.g., default: min('500',2*size_suite) ); \n"
	        + "    TcIt_max \t: number of iterations per path - only for criterion={diversity} (e.g., default: '10' ); \n"
	        + "    \n"
	        + "     - Alternatively, to run with the XML-based configuration use: java -jar EventSequenceGenerator runconfig [files.xml]\n"
	        + "    (Main class: eu.fittest.eventSequenceGenerator.Main) \n";
*/

--(1.b) using only input parameters (i.e., 3 parameters)

/**
String usage = "usage: java -jar EventSequenceGenerator inputFolder inputFolderTracess criterion \n" 
			 + "   where: \n"			
	    	 + "   inputFolder \t: path to the folder of the actual fsm (e.g., default: 'input/tudu_actual/fsm.fsm')\n"
	    	 + "   inputFolderTraces \t: path to the folder of the traces (e.g., default: 'input/logs')\n"
	         + "    criterion \t: the criterion used to extract event sequences (e.g., default: 'VISITOR_COVERAGE_UNIFORM' - VISITOR_BREADTHFIRST,VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS,VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS, VISITOR_BREADTHFIRST_2,VISITOR_BREADTHFIRST_2,VISITOR_COVERAGEWITHFRQ_LOGS, VISITOR_xLeastFrequentEven_LOGS, VISITOR_COVERAGE_UNIFORM_LOGS, VISITOR_COVERAGE_UNIFORM,VISITOR_SEQ_MAXK,VISITOR_SEQK,VISITOR_SEMK,VISITOR_SEM_MAXK,VISITOR_SEM_LastEventPair_MAXK, VISITOR_SEM_LastEventPair_K,VISITOR_ALT_MAXK,VISITOR_DIVERSITY_TC,VISITOR_DIVERSITY_TL,VISITOR_DIVERSITY_EDM,VISITOR_DIVERSITY_EDA,VISITOR_DIVERSITY_LastEventPair_TC, VISITOR_DIVERSITY_LastEventPair_TL,VISITOR_DIVERSITY_LastEventPair_EDA,VISITOR_DIVERSITY_LastEventPair_EDM); \n"
*/

--(1.c) using a configuration file

MainToImport runconfig config_sequenceExtractor.xml

example of xml 
/**
<config>
	<folderTraces></folderTraces>
	<folderOriginalFSM>input/NotePad.fsm</folderOriginalFSM>
	<typeOfCoverage>coverage_uniform_formnFSM</typeOfCoverage>
	<folderNewEventSequences>output/coverage_uniform_formnFSM</folderNewEventSequences>
	<maxLengthGeneratedTraces>100</maxLengthGeneratedTraces>
	<k>5</k>
	<typeOfVisit>givenLengthK</typeOfVisit>
	<fileNameOfAbstractTC>abstractTC</fileNameOfAbstractTC>
	<N_max>156</N_max>
	<t_initial>5</t_initial>
	<SuiteIt_max>1500</SuiteIt_max>
	<TcIt_max>20</TcIt_max>
</config>
**/

--(1.d) using the default configuration file

MainToImport runconfig 

example of default xml (named config_sequenceExtractor.xml) 
/**
<config>
	<folderTraces></folderTraces>
	<folderOriginalFSM>input/NotePad.fsm</folderOriginalFSM>
	<typeOfCoverage>coverage_uniform_formnFSM</typeOfCoverage>
	<folderNewEventSequences>output/coverage_uniform_formnFSM</folderNewEventSequences>
	<maxLengthGeneratedTraces></maxLengthGeneratedTraces>
	<k></k>
	<typeOfVisit>givenLengthK</typeOfVisit>
	<fileNameOfAbstractTC>abstractTC</fileNameOfAbstractTC>
	<N_max></N_max>
	<t_initial></t_initial>
	<SuiteIt_max></SuiteIt_max>
	<TcIt_max></TcIt_max>
</config>
**/

--(1.e)  using the default parameter values

MainToImport 

----------------------------- (2) from another Java software

--(2.a)  main_run(String[] args)

 to execute the software using one of the five possibilities of 1 (from 1.a to 1.e).

 --(2.b) run() 
 
 to execute the software using the default xml (as well as 1.d), if any, or with default parameter values (i.e., 1.e)
 
 -----------------------------