/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.fsm.visitor;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import eu.fbk.se.fsm.*;
import eu.fbk.se.fsm.visitor.IFSMVisitor;
import eu.fittest.eventSequenceGenerator.MainToImport;

public class EventSequenceGenerator_Proxy extends MainToImport implements IFSMVisitor{
	
	public Map<String,Edge> FSM_edges = new HashMap<String, Edge>();
	
	public MainToImport mainEventSequenceGenerator=new MainToImport(); 
	
	private Vector<Path> paths = new Vector<Path>();
	
	public Vector<Path> getPaths() {
		return paths;
	}
	
	public EventSequenceGenerator_Proxy(String visitorName,String inputModel,String folderTraces){
		//mainEventSequenceGenerator.typeOfCoverage=visitorName;
		//mainEventSequenceGenerator.folderOriginalFSM=inputModel;
		//mainEventSequenceGenerator.folderTraces=folderTraces;
		//mainEventSequenceGenerator.executionFromProxy=true;
		mainEventSequenceGenerator.setParameters_fromProxy(visitorName,inputModel,folderTraces);
	}
	
	public void visit(FSM fsm){
		extractEdges(fsm);
		visit();
	}
	
	public void visit(){
		//mainEventSequenceGenerator.folderOriginalTraces="";
		//mainEventSequenceGenerator.typeOfCoverage=;
		//mainEventSequenceGenerator.folderNewTraces="";
		//mainEventSequenceGenerator.typeOfFitness="";
		Vector<eu.fittest.eventSequenceGenerator.data.Path> pathsOld=mainEventSequenceGenerator.run();
		convertPathsFormat(pathsOld);
	}

	void convertPathsFormat(Vector<eu.fittest.eventSequenceGenerator.data.Path> pathsOld){
		eu.fittest.eventSequenceGenerator.data.Path pathOld;
		Vector<eu.fittest.eventSequenceGenerator.data.Edge> edgesOld;
		
		Path p=new Path();
		Edge e;
		Node n;
		
		paths = new Vector<Path>();

		for (int i = 0; i < pathsOld.size(); i++) {
			pathOld=pathsOld.get(i);
			p=new Path();

			for (int i3 = 0; i3 < pathOld.getEdges().size(); i3++) {
				p.add( new Edge( new Node(pathOld.getEdges().get(i3).getTarget().getLabel()), pathOld.getEdges().get(i3).getEvent()) );
			}

			if (p.getEdges().size()>0) paths.add(p);
		}
		
	}
	
	void extractEdges(FSM fsmflat){
		  Set<Edge> visited = new HashSet<Edge>();
		  FSM_edges = new HashMap<String, Edge>();
		  
		  for (Iterator<Node> it= fsmflat.getNodes().iterator(); it.hasNext(); ) {
			  	Node node = it.next();
			  	
			  	for (Edge e: node.getSucc()) {
			  		
					if (!visited.contains(e)) {
						 for (Edge e1: node.getSucc()) {
								//System.out.println("edge+="+e1.getEvent());
								FSM_edges.put(e1.getEvent(), e1);
							}
						 
					}
					visited.add(e);
				}
		  } 
	}
	
	
}
