/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
