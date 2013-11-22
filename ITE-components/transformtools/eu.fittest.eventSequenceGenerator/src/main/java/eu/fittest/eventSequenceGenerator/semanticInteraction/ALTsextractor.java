/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.eventSequenceGenerator.semanticInteraction;

import java.util.Vector;
import java.util.LinkedList;
import java.util.List;

import eu.fittest.eventSequenceGenerator.data.*;

/**
*
* @author Alessandro
*
*/
public class ALTsextractor extends SEQsextractor{
	
	Vector<Path> paths_K_alt=null;
	
	FSM fsm = null;
	Vector<Path> pathsTmp=null;
	
	@Override
	public void reset(String originalFsmPath,String typeOfVisit,int maxK){
		paths=new Vector<Path>();
		paths_K=new Vector<Path>();
		paths_K_alt=new Vector<Path>();
		k=maxK;
		fsm = new FSM(originalFsmPath);
	}
	
	@Override
	public Vector<Path> getPathsOfLength(String originalFsmPath,String typeOfVisit,int currentK){	
		if (typeOfVisit.equalsIgnoreCase("BreadthFirstVisit"))
			pathsTmp=bfv.visit(fsm,"");
		else if (typeOfVisit.equalsIgnoreCase("BreadthFirstVisitWithLoop"))
			pathsTmp=bfvwl.visit(fsm,"");
		else if (typeOfVisit.equalsIgnoreCase("BreadthFirstVisitWithGlobalLoop"))
			pathsTmp=bfvwgl.visit(fsm,"");
		else if (typeOfVisit.equalsIgnoreCase("givenLengthK"))
			pathsTmp=gpolk.visit(fsm,"",currentK);
		
		return pathsTmp;
	}
	
	@Override
	public Vector<Path> run_allSequences_maxK(String originalFsmPath,String outPathFolder, String fileName, int maxK,String typeOfVisit){
		reset(originalFsmPath,typeOfVisit,maxK);
		for (int kindex = 2; kindex < maxK+1; kindex++) {
			run_allSequences_K(false,originalFsmPath,outPathFolder,fileName,kindex,typeOfVisit);
		}
		
		wp.printPaths(paths_K_alt,outPathFolder,fileName);
		return paths_K_alt;
	}
	
	@Override
	public Vector<Path> run_allSequences_K(boolean reset,String originalFsmPath,String outPathFolder, String fileName, int maxK,String typeOfVisit){
		this.k=maxK;
		
		if (reset) reset(originalFsmPath,typeOfVisit,this.k);
		getPathsOfLength(originalFsmPath,typeOfVisit,this.k);
		Path pathTmp=null;
		
	/*	if (typeOfVisit.equalsIgnoreCase("BreadthFirstVisit"))
			pathsTmp=bfv.visit(fsm,"");
		else if (typeOfVisit.equalsIgnoreCase("BreadthFirstVisitWithLoop"))
			pathsTmp=bfvwl.visit(fsm,"");
		else if (typeOfVisit.equalsIgnoreCase("BreadthFirstVisitWithGlobalLoop"))
			pathsTmp=bfvwgl.visit(fsm,"");*/
		
		if (pathsTmp!=null) {
			for (Path path : pathsTmp) {
				pathTmp=new Path();
				pathTmp.copy(path);
				if (!exists(paths,pathTmp)) {
					paths.add(pathTmp);
				}
			}
		}
		
		System.out.println(" size of sequences = "+paths.size());
		selectPaths_OfLengthK(fsm);
		
		System.out.println(" size of sequences of length k ="+paths_K.size());
		selectALTPaths_OfLengthK(fsm);

		System.out.println(" size of sequences of length k that are alt-semantic ="+paths_K_alt.size());

		if (reset)  wp.printPaths(paths_K_alt,outPathFolder,fileName);
		
		return paths_K_alt;
		
	}
	
	Vector<Path> getCopyOf_paths_K_alt(){
		Vector<Path> copy_paths_K_alt=new Vector<Path>();
		Path p;
		for (Path path : paths_K_alt) {
			p=new Path();
			p.copy(path);
			if (!exists(copy_paths_K_alt,p)) {
				copy_paths_K_alt.add(p);
			}
		}
		return copy_paths_K_alt;
	}
	
	public Vector<Path> get_allSequences_K(boolean reset,String originalFsmPath,int maxK,String typeOfVisit){
		this.k=maxK;
		
		if (reset) reset(originalFsmPath,typeOfVisit,this.k);
		getPathsOfLength(originalFsmPath,typeOfVisit,this.k);
		Path pathTmp=null;
		
	/*	if (typeOfVisit.equalsIgnoreCase("BreadthFirstVisit"))
			pathsTmp=bfv.visit(fsm,"");
		else if (typeOfVisit.equalsIgnoreCase("BreadthFirstVisitWithLoop"))
			pathsTmp=bfvwl.visit(fsm,"");
		else if (typeOfVisit.equalsIgnoreCase("BreadthFirstVisitWithGlobalLoop"))
			pathsTmp=bfvwgl.visit(fsm,"");*/
		
		if (pathsTmp!=null) {
			for (Path path : pathsTmp) {
				pathTmp=new Path();
				pathTmp.copy(path);
				if (!exists(paths,pathTmp)) {
					paths.add(pathTmp);
				}
			}
		}
		
		selectPaths_OfLengthK(fsm);
		
		selectALTPaths_OfLengthK(fsm);

		return getCopyOf_paths_K_alt();
	}
	
	protected void selectALTPaths_OfLengthK(FSM fsm){
		Path path_copy=null;
		List<Edge> e1;
		Edge e2;
		Node s0=null;
		Node s1=null;	
		boolean issem=false;
		
		for (Path path : paths_K) {
			    
				issem=false;
				
				e1=new LinkedList<Edge>();
				if ( (path.getEdges().size())==2 ) {
					e1.add(path.getEdges().get(0));
				}else {
					for (int i = 1; i < (path.getEdges().size())-1; i++) {
						e1.add(path.getEdges().get(i));
					}
				}
				e2=path.getEdges().get((path.getEdges().size())-1);
				
				//s0=path.getEdges().get(0).getTarget();
				s0=fsm.getStartNode();
				
				s1=path.getEdges().get((path.getEdges().size())-1).getTarget();

				issem=SEMchecker.isALTSEM(fsm, e1, e2, s0, s1);
				
				if (issem){
					if (!exists(paths_K_alt,path)) {
						path_copy=new Path();
						path_copy.copy(path);
						paths_K_alt.add(path_copy);
					}
				}
			
		}
		
	}
	
}
