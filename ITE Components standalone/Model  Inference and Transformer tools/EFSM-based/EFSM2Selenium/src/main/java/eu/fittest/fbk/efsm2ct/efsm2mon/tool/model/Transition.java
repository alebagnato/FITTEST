/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.fbk.efsm2ct.efsm2mon.tool.model;

/**
 * 
 * @author tiella
 */
public class Transition {

	private State source;
	private State target;
	private Mutator mutator;

	public State getSource() {
		return source;
	}

	public void setSource(State source) {
		this.source = source;
		source.addOutgoing(this);
	}

	public State getTarget() {
		return target;
	}

	public void setTarget(State target) {
		this.target = target;
		target.addIncoming(this);
	}

	public String toString() {
		return source + "->" + target;
	}

	public Mutator getMutator() {
		return mutator;
	}

	public void setMutator(Mutator mutator) {
		this.mutator = mutator;
	}

}
