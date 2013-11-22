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
