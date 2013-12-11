eventBasedFSM infers FSMs from sequences of events. 
Given a set of event-based traces, it can infer the FSM using hte full set or it can balance among: under/over approximation and size.

The project requires eu.fittest.fsm.

The main class is: eu.fittest.eventBasedFSM.EventBasedFSM.
It contains the main method that can be executed as follows:

---- (1) infer the FSM using all the traces, with the following list of parameters: 

(*) "complete [path_to_the_directory_of_traces] [name_of_the_generated_FSM] [extension_of_the_trace_files]"

example:

(*) "complete [./input] [./output.txt] [trc]"

concrete examples:
complete ./Cyclos2/Cyclos2traces ""

Note: parameters under parenthesis can be omitted, in such a case the default values (see the examples above) will be used.

---- (2) inferring a set of FSMs optimized with respect to under/over approximation and size, with the following list of parameters: 

(*) "optimized [path_to_the_directory_of_traces] [path_to_the_directory_containing_the_generated_FSM] [extension_of_the_trace_files] [path to the XML configuration file] [type of the evolutionary algorithm to use (i.e., GA\NSGA]"

examples:

(*) "optimized [./input] [./output] [trc] [./EvoAlgConfig/ga-params.xml] [GA]"
(*) "optimized [./input] [./output] [trc] [./EvoAlgConfig/ga-params.xml] [NSGA]"

concrete example:
optimized ./Cyclos2/Cyclos2traces ./out "" "" NSGA

Note: parameters under parenthesis can be omitted, in such a case the default values (see the examples above) will be used.

---- (3) apply both (1) and (2)... 

(*) "[both] [path_to_the_directory_of_traces]"

example:

(*) "[./input]"

Note: in this last case only the 'path to the directory of traces' can be selected by the user, while the default values will be used for the other parameters 


