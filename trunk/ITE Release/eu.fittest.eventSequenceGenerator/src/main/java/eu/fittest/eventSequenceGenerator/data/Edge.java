package eu.fittest.eventSequenceGenerator.data;

import java.util.HashSet;

import java.util.Set;

/**
*
* @author Alessandro
*
*/
public class Edge {
	Node target;
	String event;
	Set<String> marks = new HashSet<String>();
	public int execFreq=0;
	
	void addMark(String m) {
		marks.add(m);
	}
	
	Set<String> getMarks() {
		return marks;
	}
	
	public Edge(Node target, String event) {
		this.target = target;
		this.event = event;
	}

	Edge(Node target, String event, Set<String> marks) {
		this.target = target;
		this.event = event;
		this.marks = marks;
	}
	
	public Node getTarget() {
		return target;
	}
	
	void setTarget(Node target) {
		this.target = target;
	}
	
	public String getEvent() {
		return event;
	}
	
}

