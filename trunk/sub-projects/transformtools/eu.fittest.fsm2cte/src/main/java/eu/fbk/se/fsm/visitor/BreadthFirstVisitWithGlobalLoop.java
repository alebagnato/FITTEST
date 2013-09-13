package eu.fbk.se.fsm.visitor;

import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

import eu.fbk.se.fsm.Edge;
import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.Node;

public class BreadthFirstVisitWithGlobalLoop implements IFSMVisitor {

	private Vector<Path> paths = new Vector<Path>();

	static public void main(String args[]) {
		if (args.length < 1) {
			System.err
					.println("Usage java eu.fbk.se.fsm.BreadthFirstVisitWithGlobalLoop model.fsm");
			System.exit(1);
		}
		FSM fsm = new FSM(args[0]);
		BreadthFirstVisitWithGlobalLoop bfv = new BreadthFirstVisitWithGlobalLoop();
		bfv.visit(fsm);
		bfv.printPaths();
	}

	private void printPaths() {
		for (Path path : paths) {
			boolean first = true;
			for (Edge e : path.getEdges()) {
				if (!first)
					System.out.print(", ");
				System.out.print(e.getEvent());
				first = false;
			}
			System.out.println();
		}
	}

	public void visit(FSM fsm) {

		Set<Edge> loopEdges = new HashSet<Edge>();

		// look for end node (Rome, everypath leads to Rome)
		Vector<Node> romeNodes = new Vector<Node>();
		for (Node n : fsm.getNodes()) {
			if (n.getSucc().isEmpty()) {
				romeNodes.add(n);
			}
		}

		if (romeNodes.isEmpty()) {
			return;
		}

		Node startNode = fsm.getStartNode();
		for (Edge e : startNode.getSucc()) {
			Path path = new Path();
			path.add(e);
			if (e.getTarget().equals(startNode)) {
				loopEdges.add(e);
			}
			paths.add(path);
		}
		visit(loopEdges, romeNodes);

	}

	private void visit(Set<Edge> loopEdges, Vector<Node> romeNodes) {
		boolean done = false;
		while (!done) {
			Vector<Path> newPaths = new Vector<Path>();
			Vector<Path> delPaths = new Vector<Path>();
			for (Path path : paths) {
				Edge e = path.getEdges().lastElement();
				boolean pathExtended = false;
				Node currentNode = e.getTarget();

				if (romeNodes.contains(currentNode))
					continue; // consider next path, this one is done

				for (Edge succ : currentNode.getSucc()) {
					if (!loopEdges.contains(succ)) {
						Path newPath = new Path();
						newPath.copy(path);
						newPath.add(succ);
						newPaths.add(newPath);
						pathExtended = true;

						// check for loop
						Node targetNode = succ.getTarget();
						if (targetNode.equals(currentNode)) {
							loopEdges.add(succ);
						} else {
							for (Edge edge : newPath.getEdges()) {
								if (!edge.equals(succ)
										&& edge.getTarget().equals(targetNode)) {
									loopEdges.add(succ);
									break; // no need to go further
								}
							}
						}

					}
				}
				if (pathExtended)
					delPaths.add(path);

			}
			for (Path path : delPaths)
				paths.remove(path);
			paths.addAll(newPaths);

			// check if done, all paths lead to Rome :-)
			done = true;
			for (Path path : paths) {
				Edge e = path.getEdges().lastElement();
				// System.out.println(e.getTarget().getLabel());
				if (!romeNodes.contains(e.getTarget())) {
					done = false;
				}
			}
		}
	}

	public Vector<Path> getPaths() {
		return paths;
	}

}
