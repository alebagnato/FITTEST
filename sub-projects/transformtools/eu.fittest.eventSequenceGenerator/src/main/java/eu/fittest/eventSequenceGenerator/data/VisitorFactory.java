/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.eventSequenceGenerator.data;

import java.util.HashMap;
import java.util.Map;


public class VisitorFactory {
	
	public static final String VISITOR_BREADTHFIRST = "BREADTHFIRST";
	public static final String VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS = "BREADTHFIRST_WITH_GLOBAL_LOOPS";
	public static final String VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS = "BREADTHFIRST_WITHL_LOCAL_LOOPS";
	public static final Map<String,String> VISITORS = new HashMap<String, String>();
	
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
		VISITORS.put("VISITOR_BREADTHFIRST", VISITOR_BREADTHFIRST);
		VISITORS.put("VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS", VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS);
		VISITORS.put("VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS", VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS);
		//Ale
		VISITORS.put("VISITOR_BREADTHFIRST_2", VISITOR_BREADTHFIRST_2);
		VISITORS.put("VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS_2", VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS_2);
		VISITORS.put("VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS_2", VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS_2);
		VISITORS.put("VISITOR_COVERAGEWITHFRQ_LOGS",VISITOR_COVERAGEWITHFRQ_LOGS);
		VISITORS.put("VISITOR_xLeastFrequentEven_LOGS",VISITOR_xLeastFrequentEven_LOGS);
		VISITORS.put("VISITOR_COVERAGE_UNIFORM_LOGS",VISITOR_COVERAGE_UNIFORM_LOGS);
		VISITORS.put("VISITOR_COVERAGE_UNIFORM",VISITOR_COVERAGE_UNIFORM);
		VISITORS.put("VISITOR_SEQ_MAXK",VISITOR_SEQ_MAXK);
		VISITORS.put("VISITOR_SEQK",VISITOR_SEQK);
		VISITORS.put("VISITOR_SEMK",VISITOR_SEMK);
		VISITORS.put("VISITOR_SEM_MAXK",VISITOR_SEM_MAXK);
		VISITORS.put("VISITOR_SEM_LastEventPair_MAXK",VISITOR_SEMsextractor_onlyLastEvent_maxK); 
		VISITORS.put("VISITOR_SEM_LastEventPair_K",VISITOR_SEMsextractor_onlyLastEvent_K); 
		VISITORS.put("VISITOR_ALT_MAXK",VISITOR_ALT_MAXK);
		VISITORS.put("VISITOR_DIVERSITY_TC",VISITOR_DIVERSITY_TC);
		VISITORS.put("VISITOR_DIVERSITY_TL",VISITOR_DIVERSITY_TL);
		VISITORS.put("VISITOR_DIVERSITY_EDM",VISITOR_DIVERSITY_EDM);
		VISITORS.put("VISITOR_DIVERSITY_EDA",VISITOR_DIVERSITY_EDA);
		VISITORS.put("VISITOR_DIVERSITY_LastEventPair_TC",VISITOR_DIVERSITY_onlyLastEvent_TC);
		VISITORS.put("VISITOR_DIVERSITY_LastEventPair_TL",VISITOR_DIVERSITY_onlyLastEvent_TL);
		VISITORS.put("VISITOR_DIVERSITY_LastEventPair_EDA",VISITOR_DIVERSITY_onlyLastEvent_EDA);
		VISITORS.put("VISITOR_DIVERSITY_LastEventPair_EDM",VISITOR_DIVERSITY_onlyLastEvent_EDM);
		
	}
	
	
}
