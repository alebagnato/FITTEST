/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.eventSequenceGenerator.data;

import java.util.Vector;

/**
*
* @author Alessandro
*
*/
public class Path {
//	protected String id;
	protected Vector<Edge> edges;
	protected Vector<Edge> loopEdges;
	
	private boolean deadEnd;
	
	public boolean isDeadEnd() {
		return deadEnd;
	}

	public void setDeadEnd(boolean deadEnd) {
		this.deadEnd = deadEnd;
	}

	public Path(){
//		id = "_p" + String.valueOf(pathCounter);
		edges = new Vector<Edge>();
		loopEdges = new Vector<Edge>();
	}

	public void copy(Path p) {
		edges.clear();
		loopEdges.clear();
		edges.addAll(p.getEdges());
		loopEdges.addAll(p.getLoopEdges());
	}

	public Vector<Edge> getEdges() {
		return edges;
	}
	
	public void setEdges(Vector<Edge> path) {
		this.edges = path;
	}
	
	public Vector<Edge> getLoopEdges() {
		return loopEdges;
	}
	
	public void setLoopEdges(Vector<Edge> loopEdges) {
		this.loopEdges = loopEdges;
	}
	
	public void add(Edge e){
		edges.add(e);
	}
	
	public void addLoopEdge(Edge e){
		loopEdges.add(e);
	}
	
	public boolean isComplete(Vector<Node> romeNodes){
		Edge e = edges.lastElement();
		Node lastNode = e.getTarget();
		
		if (romeNodes.contains(lastNode)){
			return true;
		} else {
			return false;
		}
	}
	
	//
	public boolean equals(Path p){
		if (this.getEdges().size()!=p.getEdges().size()) return false;
		if (this.getLoopEdges().size()!=p.getLoopEdges().size()) return false;
	
		for (int index = 0; index < p.getEdges().size(); index++) {
			if (!p.getEdges().get(index).getEvent().equalsIgnoreCase(this.getEdges().get(index).getEvent())){
				if (!p.getEdges().get(index).getTarget().getLabel().equalsIgnoreCase(this.getEdges().get(index).getTarget().getLabel())){
					return false;
				}
				else return false;
			}
			
			
		}
		
		for (int index = 0; index < p.getLoopEdges().size(); index++) {
		if (!p.getLoopEdges().get(index).getEvent().equalsIgnoreCase(this.getLoopEdges().get(index).getEvent())){
			if (!p.getLoopEdges().get(index).getTarget().getLabel().equalsIgnoreCase(this.getLoopEdges().get(index).getTarget().getLabel())){
				return false;
			}
			else return false;
		}
		}
		
		return true;
	}
	
}
