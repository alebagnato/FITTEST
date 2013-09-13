package eu.fbk.se.fsm.visitor;

import java.util.Vector;

import eu.fbk.se.fsm.Edge;
import eu.fbk.se.fsm.Node;

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
	
	
}
