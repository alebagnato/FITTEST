package eu.fbk.se.fsm.visitor;

import java.util.*;

import eu.fbk.se.fsm.Edge;
import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.Node;

public class BreadthFirstVisit implements IFSMVisitor{
	
	private Vector<Path> paths = new Vector<Path>();
	
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
		for (Path path: paths) {
			boolean first = true;
			for (Edge e: path.getEdges()) {
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
			Path path = new Path();
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
			Vector<Path> newPaths = new Vector<Path>();
			Vector<Path> delPaths = new Vector<Path>();
			for (Path path: paths) {
				Edge e = path.getEdges().lastElement();
				boolean pathExtended = false;
				for (Edge succ: e.getTarget().getSucc()) {
					if (!visited.contains(succ)) {
						Path newPath = new Path();
						newPath.copy(path);
						newPath.add(succ);
						newPaths.add(newPath);
						visited.add(succ);
						pathExtended = true;
						fixpoint = false;
					}
				}
				if (pathExtended) delPaths.add(path);
			}
			for (Path path: delPaths)
				paths.remove(path);
			paths.addAll(newPaths);
		}
	}

	
	public Vector<Path> getPaths() {
		return paths;
	}
	
}
