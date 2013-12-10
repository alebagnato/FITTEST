package eu.fbk.se.fsm;

import java.util.*;

public class BreadthFirstVisit {
	
	private Vector<Vector<Edge>> paths = new Vector<Vector<Edge>>();
	
	static public void main(String args[]) {
		if (args.length < 1) {
			System.err.println("Usage java eu.fbk.se.fsm.BreadthFirstVisit model.fsm");
			System.exit(1);
		}
		FSM fsm = new FSM(args[0]);
		BreadthFirstVisit bfv = new BreadthFirstVisit();
		bfv.visit(fsm);
		bfv.printPaths();
	}

	private void printPaths() {
		for (Vector<Edge> path: paths) {
			boolean first = true;
			for (Edge e: path) {
				if (!first) System.out.print(", ");
				System.out.print(e.getEvent());
				first = false;
			}
			System.out.println();
		}
	}

	public void visit(FSM fsm) {
		Set<Edge> visited = new HashSet<Edge>();
		Node startNode = fsm.getStartNode();
		for (Edge e: startNode.getSucc()) {
			Vector<Edge> path = new Vector<Edge>();
			path.add(e);
			paths.add(path);
			visited.add(e);
		}
		visit(visited);		
	}
	
	private void visit(Set<Edge> visited) {
		boolean fixpoint = false;
		while (!fixpoint) {
			fixpoint = true;
			Vector<Vector<Edge>> newPaths = new Vector<Vector<Edge>>();
			Vector<Vector<Edge>> delPaths = new Vector<Vector<Edge>>();
			for (Vector<Edge> path: paths) {
				Edge e = path.lastElement();
				boolean pathExtended = false;
				for (Edge succ: e.getTarget().getSucc()) {
					if (!visited.contains(succ)) {
						Vector<Edge> newPath = new Vector<Edge>();
						newPath.addAll(path);
						newPath.add(succ);
						newPaths.add(newPath);
						visited.add(succ);
						pathExtended = true;
						fixpoint = false;
					}
				}
				if (pathExtended) delPaths.add(path);
			}
			for (Vector<Edge> path: delPaths)
				paths.remove(path);
			paths.addAll(newPaths);
		}
	}

	
	public Vector<Vector<Edge>> getPaths() {
		return paths;
	}
	
}
