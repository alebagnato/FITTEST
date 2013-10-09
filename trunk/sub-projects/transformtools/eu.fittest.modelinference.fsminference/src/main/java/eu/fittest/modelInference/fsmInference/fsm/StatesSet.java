/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.modelInference.fsmInference.fsm;

import java.util.*;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class StatesSet {

	public Vector<Fsm> statesSet;
	public int countEventsInLogs = 0;

	public StatesSet() {
		statesSet = new Vector<Fsm>();
	}

	public Vector<Fsm> getAllStatesSet() {
		return statesSet;
	}

	public void addStates(Fsm statesToadd) {
		Fsm f = statesToadd.clone();

		statesSet.add(f);
	}

	public void addToCountEventsInLogs(int num) {
		countEventsInLogs = countEventsInLogs + num;
	}

	public void setCountEventsInLogs(int num) {
		countEventsInLogs = num;
	}

	public int getCountEventsInLogs() {
		return countEventsInLogs;
	}
}
