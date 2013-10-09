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
public class Unique {
	private static Unique instance = null;

	Vector<Long> uniqueIdStates;
	Vector<Long> uniqueIdTransitions;

	private Unique() {
	}

	public static Unique getInstace() {
		if (instance == null) {
			instance = new Unique();
			instance.uniqueIdStates = new Vector<Long>();
			instance.uniqueIdTransitions = new Vector<Long>();
		}
		return instance;
	}

	public boolean iscontainedState(long valueToSearch) {
		for (int i = 0; i < instance.uniqueIdStates.size(); i++) {
			if (instance.uniqueIdStates.get(i).intValue() == valueToSearch) {
				return true;
			}
		}
		return false;
	}

	public boolean iscontainedTransitions(long valueToSearch) {
		for (int i = 0; i < instance.uniqueIdTransitions.size(); i++) {
			if (instance.uniqueIdTransitions.get(i).intValue() == valueToSearch) {
				return true;
			}
		}
		return false;
	}

	public void reset() {
		instance = new Unique();
		instance.uniqueIdStates = new Vector<Long>();
		instance.uniqueIdTransitions = new Vector<Long>();
	}

	public Vector<Long> getUniqueIdStates() {
		return instance.uniqueIdStates;
	}

	public void setUniqueIdStates(Vector<Long> uniqueIdStates) {
		instance.uniqueIdStates = uniqueIdStates;
	}

	public void addToUniqueIdStates(long uniqueIdStates) {
		instance.uniqueIdStates.add(new Long(uniqueIdStates));
	}

	public void addToUniqueIdTransitions(long uniqueIdTransitions) {
		instance.uniqueIdTransitions.add(new Long(uniqueIdTransitions));
	}

	public Vector<Long> getUniqueIdTransitions() {
		return instance.uniqueIdTransitions;
	}

	public void setUniqueIdTransitions(Vector<Long> uniqueIdTransitions) {
		instance.uniqueIdTransitions = uniqueIdTransitions;
	}

}
