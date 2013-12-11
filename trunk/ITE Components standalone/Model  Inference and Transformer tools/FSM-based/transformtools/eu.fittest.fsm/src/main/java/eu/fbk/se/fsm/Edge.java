/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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

