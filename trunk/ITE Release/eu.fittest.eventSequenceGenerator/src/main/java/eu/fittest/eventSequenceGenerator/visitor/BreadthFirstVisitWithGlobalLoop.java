
package eu.fittest.eventSequenceGenerator.visitor;

import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

import eu.fittest.eventSequenceGenerator.data.Edge;
import eu.fittest.eventSequenceGenerator.data.FSM;
import eu.fittest.eventSequenceGenerator.data.Node;
import eu.fittest.eventSequenceGenerator.data.Path;
import eu.fittest.eventSequenceGenerator.data.WritePaths;
import eu.fittest.eventSequenceGenerator.utility.PathsUtils;

public class BreadthFirstVisitWithGlobalLoop {

	private Vector<Path> paths = new Vector<Path>();
	private Vector<Path>  paths2 = new Vector<Path>();
	WritePaths wp=new WritePaths();
	PathsUtils pathUtils=new PathsUtils();
	
	public void checkContentOfPaths(){
		paths2 = new Vector<Path>();
		for (Path path : paths) {
			if (!pathUtils.isContained(paths2, path)) paths2.add(path);
		}
		
		paths = new Vector<Path>();
		for (Path path : paths2) {
			paths.add(path);
		}
	}

	public Vector<Path> run(String pathFolder,String outPathFolder,String fileName){
		FSM fsm = new FSM(pathFolder);
		visit(fsm);
		checkContentOfPaths();
		wp.printPaths(paths,outPathFolder,fileName);
		return paths;
	}
	
	public Vector<Path> visit(FSM fsm,String type){
		visit(fsm);
		checkContentOfPaths();
		return paths;
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
