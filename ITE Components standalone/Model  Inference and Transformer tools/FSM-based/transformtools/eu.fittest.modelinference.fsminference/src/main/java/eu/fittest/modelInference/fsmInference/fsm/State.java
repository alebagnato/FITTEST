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

import eu.fittest.modelInference.fsmInference.utility.Utility;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class State {
	long id = -1;
	String[] stateContent;
	Utility utils = new Utility();
	Unique uniqueIds = Unique.getInstace();

	public State(String[] stateContent) {

		long tmp = 0;
		tmp = utils.randomLong();
		while (uniqueIds.iscontainedState(tmp)) {
			tmp = utils.randomLong();
		}
		uniqueIds.addToUniqueIdStates(tmp);
		setId(tmp);
		setStateContent(stateContent);
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String[] getStateContent() {
		return stateContent;
	}

	public void setStateContent(String[] stateContent) {
		this.stateContent = utils.copy(stateContent);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (this == null)
			return false;
		if (!(o instanceof State))
			return false;
		boolean eq = false;
		if (((State) o).getId() == this.getId())
			eq = true;
		else
			eq = false;
		String[] stO = ((State) o).getStateContent();
		String[] stThis = this.getStateContent();
		if (stO.length != stThis.length)
			eq = false;
		else {
			for (int i = 0; i < stO.length; i++) {
				if (stO[i].equals(stThis[i]))
					eq = true;
				else {
					eq = false;
					break;
				}
			}
		}
		return eq;
	}

	@Override
	public State clone() {
		String[] ttr = utils.copy(this.stateContent);
		State clone = new State(ttr);
		clone.setId(this.id);
		return clone;
	}
}
