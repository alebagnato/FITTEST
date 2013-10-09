/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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
