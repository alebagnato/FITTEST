package eu.fbk.se.fsm;

import java.util.HashSet;
import java.util.Set;

public class Edge {
	Node target;
	String event;
	Set<String> marks = new HashSet<String>();
	
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

	public Edge(Node target, String event, Set<String> marks) {
		this.target = target;
		this.event = event;
		this.marks = marks;
	}
	
	public Node getTarget() {
		return target;
	}
	
	public void setTarget(Node target) {
		this.target = target;
	}
	
	public String getEvent() {
		return event;
	}
	public void setEvent(String event) {
		this.event = event;
	}
}

