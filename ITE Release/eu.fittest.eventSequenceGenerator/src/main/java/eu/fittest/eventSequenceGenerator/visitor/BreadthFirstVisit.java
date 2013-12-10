package eu.fittest.eventSequenceGenerator.visitor;

import java.util.*;


import eu.fittest.eventSequenceGenerator.data.Edge;
import eu.fittest.eventSequenceGenerator.data.FSM;
import eu.fittest.eventSequenceGenerator.data.Node;
import eu.fittest.eventSequenceGenerator.data.Path;
import eu.fittest.eventSequenceGenerator.data.WritePaths;
import eu.fittest.eventSequenceGenerator.utility.PathsUtils;

public class BreadthFirstVisit{
	
	PathsUtils pathUtils=new PathsUtils();
	WritePaths wp=new WritePaths();
	
	private Vector<Path> paths = new Vector<Path>();
	private Vector<Path> paths2 = new Vector<Path>();
	
	public void checkContentOfPaths(){
		paths2 = new Vector<Path>();
		for (Path path : paths) {
			if (!pathUtils.isContained(paths2, path)) paths2.add(path);
		}
		
		paths = new Vector<Path>();
		Path p;
		for (Path path : paths2) {
			//p=new Path();
			//path.copy(p);
			paths.add(path);
		}
	}
	

	public Vector<Path> run(String pathFolder,String outPathFolder,String fileName){
		FSM fsm = new FSM(pathFolder);
		//fsm.print();
		visit(fsm);
		System.out.println(paths.size());
		checkContentOfPaths();
		System.out.println(paths.size());
		wp.printPaths(paths,outPathFolder,fileName);
		return paths;
	}
	
	public Vector<Path> run(String pathFolder){
		FSM fsm = new FSM(pathFolder);
		visit(fsm);
		checkContentOfPaths();
		return paths;
	}	
	
	public Vector<Path> visit(FSM fsm,String type){
		visit(fsm);
		checkContentOfPaths();
		return paths;
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
