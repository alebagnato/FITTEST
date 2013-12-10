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

import eu.fittest.eventSequenceGenerator.data.FSM;
import eu.fittest.eventSequenceGenerator.data.Path;
import eu.fittest.eventSequenceGenerator.data.WritePaths;
import eu.fittest.eventSequenceGenerator.visitor.BreadthFirstVisitWithGlobalLoop;
import eu.fittest.eventSequenceGenerator.visitor.BreadthFirstVisitWithLoop;
import eu.fittest.eventSequenceGenerator.visitor.BreadthFirstVisit;
import eu.fittest.eventSequenceGenerator.visitor.GetPathsOfLengthK;

/**
*
* @author Alessandro
*
*/
public class SEQsextractor {
	BreadthFirstVisit bfv=new BreadthFirstVisit();
	BreadthFirstVisitWithLoop bfvwl=new BreadthFirstVisitWithLoop();
	BreadthFirstVisitWithGlobalLoop bfvwgl=new BreadthFirstVisitWithGlobalLoop();
	GetPathsOfLengthK gpolk=new GetPathsOfLengthK();
	
	Vector<Path> paths=null;
	Vector<Path> paths_K=null;
	int k=2;
	WritePaths wp=new WritePaths();
	FSM fsm = null;
	Vector<Path> pathsTmp=null;
	
	public void reset(String originalFsmPath,String typeOfVisit,int maxK){
		paths=new Vector<Path>();
		paths_K=new Vector<Path>();
		k=maxK;
		fsm = new FSM(originalFsmPath);
	}
		
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
	
	public Vector<Path> run_allSequences_maxK(String originalFsmPath,String outPathFolder, String fileName, int maxK,String typeOfVisit){
		reset(originalFsmPath,typeOfVisit,maxK);
		
		for (int ik = 2; ik < maxK+1; ik++) {
			run_allSequences_K(false,originalFsmPath,outPathFolder,fileName,ik,typeOfVisit);
		}
		
		wp.printPaths(paths_K,outPathFolder,fileName);
		
		return paths_K;
	}
	
	public Vector<Path> run_allSequences_K(boolean reset,String originalFsmPath,String outPathFolder, String fileName, int k,String typeOfVisit){
		this.k=k;

		if (reset) reset(originalFsmPath,typeOfVisit,this.k);
		
		getPathsOfLength(originalFsmPath,typeOfVisit,this.k);
		Path pathTmp=null;
			
		if (pathsTmp!=null) {
			paths=new Vector<Path>();
			for (Path path : pathsTmp) {
				pathTmp=new Path();
				pathTmp.copy(path);
				if (!exists(paths,pathTmp)) {
					paths.add(pathTmp);
				}
			}
			
			selectPaths_OfLengthK(fsm);
		
			if (reset) wp.printPaths(paths_K,outPathFolder,fileName);
		}
		
		return paths_K;
	}
	
	protected void selectPaths_OfLengthK(FSM fsm){
		Path path_copy=null;
		for (Path path : paths) {
	
			if (path.getEdges().size()==k){	
				if (!exists(paths_K,path)) {
					path_copy=new Path();
					path_copy.copy(path);
					paths_K.add(path_copy);
				}
			}	
		}
	
	}
	
	
	protected boolean exists(Vector<Path> paths, Path ptobechecked){
		for (Path path : paths) {
			if (path.equals(ptobechecked)) return true;
		}
		return false;
	}
	
	/**
	 * To get sem sequences
	 * @param reset
	 * @param originalFsmPath
	 * @param k
	 * @param typeOfVisit
	 * @return
	 */
	public Vector<Path> get_allSequences_K(boolean reset,String originalFsmPath,int k,String typeOfVisit){
		this.k=k;
			
		if (reset) reset(originalFsmPath,typeOfVisit,this.k);
		getPathsOfLength(originalFsmPath,typeOfVisit,this.k);
		Path pathTmp=null;

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

		return paths_K;
	}
	
}
