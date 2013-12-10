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

import java.util.Vector;

import eu.fittest.modelInference.fsmInference.utility.Utility;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class States {

	Vector<State> states;
	Utility utils = new Utility();

	public States() {
		states = new Vector<State>();
	}

	public Vector<State> getStates() {
		return states;
	}

	public void addStates(State state) {
		if (exists(state.stateContent) == -1)
			states.add(state);
	}

	public int size() {
		return states.size();
	}

	public long exists(String[] stateContent) {
		State s;
		String[] stC;

		for (int i = 0; i < size(); i++) {
			s = states.get(i);
			stC = s.getStateContent();
			if (utils.equalContent(stateContent, stC))
				return s.getId();

		}
		return -1;
	}

	public State getStateById(long id) {
		State s;
		for (int i = 0; i < size(); i++) {
			s = states.get(i);
			if (s.getId() == id)
				return s;
		}

		return null;
	}

	public void removeStateById(long id) {
		State s;
		int toberemoved = -1;

		for (int i = 0; i < states.size(); i++) {
			s = states.get(i);
			if (s.getId() == id) {
				toberemoved = i;
				break;
			}
		}
		if (toberemoved > -1)
			states.remove(toberemoved);
	}

	public long getHigherStateID() {
		State s;
		long higher = -1;
		for (int i = 0; i < size(); i++) {
			s = states.get(i);
			if (s.getId() > higher)
				higher = s.getId();

		}
		return higher;
	}
}
