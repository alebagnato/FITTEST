/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
