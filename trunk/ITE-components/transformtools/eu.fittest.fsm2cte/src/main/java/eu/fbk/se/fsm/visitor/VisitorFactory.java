/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.fsm.visitor;

import java.util.HashMap;
import java.util.Map;


public class VisitorFactory {
	public static final Map<String,String> VISITORS = new HashMap<String, String>();
	
	public static final String VISITOR_BREADTHFIRST = "breadthFirst";
	public static final String VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS = "breadthFirstWithGlobalLoops";
	public static final String VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS = "breadthFirstWithLocalLoops";
	
	public static final String VISITOR_BREADTHFIRST_2="visit"; //per ?
	public static final String VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS_2 = "visitWithGlobalLoop";
	public static final String VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS_2 = "visitWithLoop";
	public static final String VISITOR_COVERAGEWITHFRQ_LOGS="coverage"; //per coverage
	public static final String VISITOR_xLeastFrequentEven_LOGS="xLeastFrequentEven";  //per coverage
	public static final String VISITOR_COVERAGE_UNIFORM_LOGS="coverage_uniform"; //per coverage, da fsm via log
	public static final String VISITOR_COVERAGE_UNIFORM="coverage_uniform_fromFSM"; //per coverage, da fsm senza passare dai log
	public static final String VISITOR_SEQ_MAXK="sequences_maxK"; //per SEM
	public static final String VISITOR_SEQK="sequences_K"; //per SEM
	public static final String VISITOR_SEMK="sem_K"; //per SEM
	public static final String VISITOR_SEM_MAXK="sem_maxK"; //per SEM
	public static final String VISITOR_SEMsextractor_onlyLastEvent_maxK="SEMsextractor_onlyLastEvent_maxK";
	public static final String VISITOR_SEMsextractor_onlyLastEvent_K="SEMsextractor_onlyLastEvent_K";
	public static final String VISITOR_ALT_MAXK="alt_maxK"; //per SEM
	public static final String VISITOR_DIVERSITY="diversity"; //per DIVERSITY
	public static final String VISITOR_DIVERSITY_TC="diversityTC";
	public static final String VISITOR_DIVERSITY_TL="diversityTL";
	public static final String VISITOR_DIVERSITY_EDM="diversityEDM";
	public static final String VISITOR_DIVERSITY_EDA="diversityEDA";
	public static final String VISITOR_DIVERSITY_onlyLastEvent_TC="diversity_onlyLastEvent_TC";
	public static final String VISITOR_DIVERSITY_onlyLastEvent_TL="diversity_onlyLastEvent_TL";
	public static final String VISITOR_DIVERSITY_onlyLastEvent_EDA="diversity_onlyLastEvent_EDA";
	public static final String VISITOR_DIVERSITY_onlyLastEvent_EDM="diversity_onlyLastEvent_EDM";

	static {
		
		VISITORS.put("Breadthfirst", VISITOR_BREADTHFIRST);
		VISITORS.put("Breadthfirst with loop (global)", VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS);
		VISITORS.put("Breadthfirst with loop (local)", VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS);
		//Ale
//		VISITORS.put("VISITOR_BREADTHFIRST_2", VISITOR_BREADTHFIRST_2);
//		VISITORS.put("VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS_2", VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS_2);
//		VISITORS.put("VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS_2", VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS_2);
		
		VISITORS.put("Coverage with frequency",VISITOR_COVERAGEWITHFRQ_LOGS);
		VISITORS.put("Coverage with least frequency",VISITOR_xLeastFrequentEven_LOGS);
//		VISITORS.put("Uniform coverage",VISITOR_COVERAGE_UNIFORM_LOGS);
		VISITORS.put("Uniform coverage",VISITOR_COVERAGE_UNIFORM);
		VISITORS.put("Sequency with Max length",VISITOR_SEQ_MAXK);
		VISITORS.put("Sequency with a fixed length",VISITOR_SEQK);
		VISITORS.put("Sematics with a fixed length",VISITOR_SEMK);
		VISITORS.put("Sematics with Max length",VISITOR_SEM_MAXK);
		VISITORS.put("Sematics (with last event pair & Max length)",VISITOR_SEMsextractor_onlyLastEvent_maxK);  
		VISITORS.put("Sematics (with last event pair & fixed length)",VISITOR_SEMsextractor_onlyLastEvent_K); 
		VISITORS.put("Alternative sematics with Max length",VISITOR_ALT_MAXK);
		VISITORS.put("Coverage-based diversity",VISITOR_DIVERSITY_TC);
		VISITORS.put("Sequence-length-based diversity",VISITOR_DIVERSITY_TL);
		VISITORS.put("EDM diversity",VISITOR_DIVERSITY_EDM);
		VISITORS.put("EDA diversity",VISITOR_DIVERSITY_EDA);
		VISITORS.put("Last-event-pair-coverage-based diversity",VISITOR_DIVERSITY_onlyLastEvent_TC);
		VISITORS.put("Last-event-pair-length-based diversity",VISITOR_DIVERSITY_onlyLastEvent_TL);
		VISITORS.put("Last-event-pair-EDA-based diversity",VISITOR_DIVERSITY_onlyLastEvent_EDA);
		VISITORS.put("Last-event-pair-EDM-based diversity",VISITOR_DIVERSITY_onlyLastEvent_EDM);
	}
	
	public static IFSMVisitor getVisitor(String visitorID, String inputModel,String folderTraces){
		if (VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS.equals(visitorID)){
			return new BreadthFirstVisitWithGlobalLoop();
		} else if (VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS.equals(visitorID)){
			return new BreadthFirstVisitWithLoop();
		} else if (VISITOR_BREADTHFIRST.equals(visitorID)){
			return new BreadthFirstVisit();
		} else {
			return new EventSequenceGenerator_Proxy(visitorID,inputModel,folderTraces);
		}	 
	}
	
}
