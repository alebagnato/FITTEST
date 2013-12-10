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
import java.util.Hashtable;
import java.util.Map;

/**
*
* @author Alessandro
*
*/
public  enum TypeOfCoverage {
		coverage, 	//per coverage, logs
		xLeastFrequentEven,	//per coverage, logs
		coverage_uniform,	//per coverage, logs
		coverage_uniform_fromFSM, //per coverage no-logs
		visit,	 //per visiting
		visitWithLoop,	 //per visiting
		visitWithGlobalLoop,	 //per visiting
		sequences_maxK,	//per SEM
		sequences_K,	//per SEM
		sem_K,	//per SEM
		sem_maxK,	//per SEM
		SEMsextractor_onlyLastEvent_maxK,  //per SEM
		SEMsextractor_onlyLastEvent_K,  //per SEM
		alt_maxK,	//per SEM 
		alt_K, 	//per SEM
		diversity, 	//per DIVERSITY
		diversityTC,
		diversityTL,
		diversityEDM,
		diversityEDA,
		diversity_onlyLastEvent, //per DIVERSITY
		diversity_onlyLastEvent_TC,
		diversity_onlyLastEvent_TL,
		diversity_onlyLastEvent_EDA,
		diversity_onlyLastEvent_EDM,
		//Cu
		BREADTHFIRST, //Cu
		BREADTHFIRST_WITH_GLOBAL_LOOPS, //Cu
		BREADTHFIRST_WITHL_LOCAL_LOOPS, //Cu
}

