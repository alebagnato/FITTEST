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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * 
 * @author tiella
 */
public class Model {

	private String sutPackage;
	private String sutName;
	private List<State> states = new ArrayList<State>();
	private List<Transition> transitions = new ArrayList<Transition>();
	private List<Mutator> mutators = new ArrayList<Mutator>();
	private List<Inspector> inspectors = new ArrayList<Inspector>();
	private State initialState;
	private boolean hasRunControl = false;

	public void setClassPath(String fullClassName) {

		if (fullClassName.contains(".")) {

			sutName = fullClassName.substring(fullClassName.lastIndexOf('.') + 1);
			sutPackage = fullClassName.substring(0, fullClassName.lastIndexOf('.'));

		} else {
			sutName = fullClassName;
			sutPackage = "";
		}

	}

	public String getSutName() {
		return sutName;
	}

	public void setSutName(String sutName) {
		this.sutName = sutName;
	}

	public boolean addState(State e) {
		return states.add(e);
	}

	public boolean addAllStates(Collection<? extends State> c) {
		return states.addAll(c);
	}

	public boolean addTransition(Transition e) {
		return transitions.add(e);
	}

	public boolean addAllTransition(Collection<? extends Transition> c) {
		return transitions.addAll(c);
	}

	public boolean addMutator(Mutator e) {
		return mutators.add(e);
	}

	public boolean addAllMutators(Collection<? extends Mutator> c) {
		return mutators.addAll(c);
	}

	public List<State> getStates() {
		return states;
	}

	public String getStateName(int stateId) {

		return states.get(stateId).getName();

	}

	public List<Transition> getTransitions() {
		return transitions;
	}

	public List<Mutator> getMutators() {
		return mutators;
	}

	public boolean addInspector(Inspector e) {
		return inspectors.add(e);
	}

	public boolean addAllInspectors(Collection<? extends Inspector> c) {
		return inspectors.addAll(c);
	}

	public List<Inspector> getInspectors() {
		return inspectors;
	}

	public String getStatesList() {
		StringBuilder sb = new StringBuilder();

		int k = 1;

		for (State s : states) {

			sb.append(s.getName());
			if (k < states.size()) {
				sb.append(", ");
			}

			k++;

		}

		return sb.toString();
	}

	public State getInitialState() {
		return initialState;
	}

	public void setInitialState(State intitialState) {
		this.initialState = intitialState;
	}

	public String getSutPackage() {
		return sutPackage;
	}

	public void setSutPackage(String sutPackage) {
		this.sutPackage = sutPackage;
	}

	public State getStateByName(String string) {

		// TODO replace this naive linear search implementation

		State found = null;

		for (State s : states) {

			if (s.getName().equals(string)) {
				found = s;
				break;
			}

		}

		return found;
	}

	public boolean isHasRunControl() {
		return hasRunControl;
	}

	public void setHasRunControl(boolean hasRunControl) {
		this.hasRunControl = hasRunControl;
	}

	public boolean checkSemantic(StringBuilder annotation) {

		if (getInitialState() == null) {

			annotation.append("missing initial state");
			return false;
		}

		return true;

	}

}
