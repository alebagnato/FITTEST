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
import java.util.List;

/**
 * 
 * @author tiella
 */
public class State {

	private String predicate;
	private boolean initial;
	private int id;
	private String name;
	private List<Transition> incoming = new ArrayList<Transition>();
	private List<Transition> outgoing = new ArrayList<Transition>();

	public String getPredicate() {
		return predicate;
	}
	
	/**
	 * dotty requires some character to be escaped
	 * @return
	 */
	public String getDottyPredicate() {
		String s = predicate.replaceAll("([<>])", "\\\\$1");
		return s.replaceAll("&&","&&\\\\n");
	}

	public void setPredicate(String p) {
		this.predicate = p;
	}

	public boolean isInitial() {
		return initial;
	}

	public void setInitial(boolean initial) {
		this.initial = initial;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public String getName() {

		return name;
	}

	public void setName(String name) {

		this.name = name;
	}

	@Override
	public String toString() {
		return "State(" + id + ")";
	}

	public boolean addIncoming(Transition e) {
		return incoming.add(e);
	}

	public boolean addOutgoing(Transition e) {
		return outgoing.add(e);
	}

	public List<Transition> getIncoming() {
		return incoming;
	}

	public List<Transition> getOutgoing() {
		return outgoing;
	}

}
