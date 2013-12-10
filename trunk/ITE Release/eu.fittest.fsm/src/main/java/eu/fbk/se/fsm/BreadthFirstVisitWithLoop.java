package eu.fbk.se.fsm;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

import eu.fbk.se.fsm.Edge;
import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.Node;

/**
 * Visit FSM, 1 visit for each loop
 * 
 * @since 10 June 2011
 * @author cunduy
 *
 */

public class BreadthFirstVisitWithLoop {
	
	private static final int MAX_NUMBER_OF_COMPLETE_PATHS = 500;
	private static final int MAX_NUMBER_OF_PATHS = 1000;
	
	private Vector<Path> paths = new Vector<Path>();
	private Vector<Node> romeNodes = new Vector<Node>();
	
	static public void main(String args[]) {
		if (args.length < 1) {
			System.err.println("Usage java eu.fbk.se.fsm.BreadthFirstVisitWithLoop model.fsm");
			System.exit(1);
		}
		FSM fsm = new FSM(args[0]);
		BreadthFirstVisitWithLoop bfv = new BreadthFirstVisitWithLoop();
//		fsm.print();
		bfv.visit(fsm);
		bfv.printPaths();
	}

	private void printPaths() {
		Vector<Path> ps = getPaths();
		for (Path p: ps) {
			printPath(p);
		}
		System.out.println("Total number of complete paths: " + ps.size());
		System.out.println("Total number of paths: " + paths.size());
	}
	
	private void printPath(Path p){
		boolean first = true;
		for (Edge e: p.getEdges()) {
			if (!first) System.out.print(", ");
			System.out.print(e.getEvent());
			first = false;
		}
		System.out.println();
	}
	
	/*
	 
	  private void printPathsToFiles() throws IOException {
		int i = 0;
		for (Vector<Edge> path: paths) {
			String newFileName = "log_" + String.valueOf(i++);
			File f = new File("/home/cunduy/workspace/cyclos_3.6_RC1/etc/cyclos/traces/" + newFileName);
			FileWriter out = new FileWriter(f);
			boolean first = true;
			for (Edge e: path) {
				//if (!first) System.out.print(", ");
				//System.out.print(e.getEvent());
				//first = false;
				out.write(e.getEvent() + "\n");
				
			}
//			out.write("\n");
			out.close();
		}
	}
	
	private void printFSMConstraints(FSM fsm){
		if (fsm == null) return;
		Vector<Node> romeNodes = new Vector<Node>();
		for (Node n : fsm.getNodes()){
			for (Edge e: n.getSucc()) {
				for (Edge s : e.getTarget().getSucc()){
					System.out.println(e.getEvent() + " :_: " + s.getEvent());
				}
			}
		}
	}
	  
	 */
	

	public void visit(FSM fsm) {

		// look for end node (Rome, every path leads to Rome)
		for (Node n : fsm.getNodes()){
			if (n.getSucc().isEmpty()){
				romeNodes.add(n);
			}
		}
		
		if (romeNodes.isEmpty()){
			return;
		}
				
		Node startNode = fsm.getStartNode();
		for (Edge e: startNode.getSucc()) {
			Path p = new Path();
			p.add(e);
			if (e.getTarget().equals(startNode)){
				p.addLoopEdge(e);
			}
			paths.add(p);
		}
		visit(romeNodes);		
		
	}
	
	private void visit(Vector<Node> romeNodes) {
		boolean done = false;
		while (!done) {
			Vector<Path> newPaths = new Vector<Path>();
			Vector<Path> delPaths = new Vector<Path>();
			for (Path p: paths) {
				Edge e = p.getEdges().lastElement();
				boolean pathExtended = false;
				Node currentNode = e.getTarget();
				
				if (romeNodes.contains(currentNode))
					continue; // consider next path, this one is done
				
				boolean isDeadEnd = true;
				for (Edge succ: currentNode.getSucc()) {
					if (!p.getLoopEdges().contains(succ) 
							&& !p.getEdges().contains(succ)) {
						// No more loop, no revisit
						Path newPath = createNewPath(p, currentNode, succ);
						newPaths.add(newPath);
						pathExtended = true;
						isDeadEnd = false;
						
					}
				}
				
				if (isDeadEnd) {
				// Path is not extended because all loops and all edges have been visited
					// at least once. Choose a first, none-loop edge to continue
					for (Edge succ: currentNode.getSucc()) {
						if (!p.getLoopEdges().contains(succ)){ 
							Path newPath = createNewPath(p, currentNode, succ);
							newPaths.add(newPath);
							pathExtended = true;
							isDeadEnd = false;
							break;
						}
					}
					
					// still dead-ended
					if (isDeadEnd){
						p.setDeadEnd(true);
					}
				}
				
				if (pathExtended) delPaths.add(p);
				
//				printPath(p);
			}
			for (Path path: delPaths)
				paths.remove(path);
			
			// add to the maximum
			int counter = paths.size();
			int i = 0;		
			
			while (counter < MAX_NUMBER_OF_PATHS && i < newPaths.size()){
				paths.add(newPaths.get(i++));
				counter++;
			}

			if (counter >= MAX_NUMBER_OF_PATHS){
				done = true;
			} else {
				// check if done, all paths must lead to Rome :-)
				done = true;
				counter = 0;
				for (Path p: paths) {
					boolean complete = p.isComplete();
					if (!complete && !p.isDeadEnd()){
						done = false;
					} else if (complete) {
						counter++;
					}
				}
				
			}

			if (counter >= MAX_NUMBER_OF_COMPLETE_PATHS){
				done = true;
			} 
			
		}
	}

	/**
	 * Create a new path from the currentPath, node and edge
	 * @param currentPath
	 * @param currentNode
	 * @param succ
	 * @return
	 */
	private Path createNewPath(Path currentPath, Node currentNode, Edge succ){
		Path newPath = new Path();
		newPath.copy(currentPath);
		newPath.add(succ);
		
		// check for loop 
		Node targetNode = succ.getTarget();
		if (targetNode.equals(currentNode)){
			// self-loop
			newPath.addLoopEdge(succ);
		} else {
			for (Edge edge : newPath.getEdges()){
				if (!edge.equals(succ) 
						&& edge.getTarget().equals(targetNode)){
					newPath.addLoopEdge(succ);
					break; // no need to go further
				}
			}
		}
		
		return newPath;
	}

	public Vector<Path> getAllPaths(){
		return paths;
	}
	
	public Vector<Path> getPaths() {
		if (romeNodes.isEmpty()){
			return paths;
		}
		
		// return only complete path
		Vector<Path> retPaths = new Vector<BreadthFirstVisitWithLoop.Path>();
		for (Path p: paths) {
			if (p.isComplete()){
				retPaths.add(p);
			}
		}
		
		return retPaths;
	}
	
	/**
	 * Class that represent a path
	 * @author cunduy
	 *
	 */
	public class Path{
		
//		protected String id;
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
//			id = "_p" + String.valueOf(pathCounter);
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
		
		public boolean isComplete(){
			Edge e = edges.lastElement();
			Node lastNode = e.getTarget();
			
			if (romeNodes.contains(lastNode)){
				return true;
			} else {
				return false;
			}
		}
		
		
	}
	
}
