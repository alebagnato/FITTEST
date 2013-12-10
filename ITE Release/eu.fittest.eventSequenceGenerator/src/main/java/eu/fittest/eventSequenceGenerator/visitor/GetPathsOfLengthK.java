package eu.fittest.eventSequenceGenerator.visitor;

import java.util.Vector;


import java.util.List;

import eu.fittest.eventSequenceGenerator.data.Edge;
import eu.fittest.eventSequenceGenerator.data.FSM;
import eu.fittest.eventSequenceGenerator.data.Node;
import eu.fittest.eventSequenceGenerator.data.Path;
import eu.fittest.eventSequenceGenerator.data.WritePaths;
import eu.fittest.eventSequenceGenerator.utility.PathsUtils;

/**
*
* @author Alessandro
*
*/
public class GetPathsOfLengthK {
	
	WritePaths wp=new WritePaths();
	int lengthK=2;
	private Vector<Path> paths = new Vector<Path>();
	PathsUtils pathUtils=new PathsUtils();
	
	public Vector<Path> visit(FSM fsm,String type,int lengthK){
		paths = new Vector<Path>();
		if (lengthK>=2) this.lengthK=lengthK;
		visitK1(fsm);
		return paths;
	}
	
	public void visitK1(FSM fsm) {
		Node startNode = fsm.getStartNode();
//		fsm.print();
		paths = new Vector<Path>();
		for (Edge e: startNode.getSucc()) {
			Path path = new Path();
			path.add(e);
			if (!pathUtils.exists(paths,path)) {
				paths.add(path);
			}
		}
		for (int currentLength = 1; currentLength < lengthK; currentLength++) {
			visitKnext(currentLength);	
		}
	}
	
	private void visitKnext(int currentLength) {
		List<Edge> outsOfLastEdge;
		Vector<Path> pathsNew=new Vector<Path>();
		Path p;
		
		for (Path path : paths) {
			if (path.getEdges().size()==currentLength) {
				if (!pathUtils.exists(pathsNew,path)) pathsNew.add(path);
			}
		}

		for (Path path : pathsNew) {
			outsOfLastEdge=path.getEdges().get(path.getEdges().size()-1).getTarget().getSucc();
			for (Edge edge : outsOfLastEdge) {
				p = new Path();
				p.copy(path);
				p.add(edge);
				if (!pathUtils.exists(paths,p)) {
					paths.add(p);
				}
			}
		}

		
	}
	
}
