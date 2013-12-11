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


import eu.fittest.eventSequenceGenerator.data.*;

/**
*
* @author Alessandro
*
*/
public class SEMsextractor extends SEQsextractor{
	
	Vector<Path> paths_K_sem=null;
	FSM fsm = null;
	Vector<Path> pathsTmp=null;
		
	@Override
	public void reset(String originalFsmPath,String typeOfVisit,int maxK){
		paths=new Vector<Path>();
		paths_K=new Vector<Path>();
		paths_K_sem=new Vector<Path>();
		pathsTmp=new Vector<Path>();
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
		for (int k = 2; k < maxK+1; k++) {
			run_allSequences_K(false,originalFsmPath,outPathFolder,fileName,k,typeOfVisit);
		}
		wp.printPaths(paths_K_sem,outPathFolder,fileName);
		
		return paths_K_sem;
	}
	
	@Override
	public Vector<Path> run_allSequences_K(boolean reset,String originalFsmPath,String outPathFolder, String fileName, int k,String typeOfVisit){
		this.k=k;
		Path pathTmp;
		
		System.out.println("K ="+this.k);
		
		if (reset) reset(originalFsmPath,typeOfVisit,this.k);
		getPathsOfLength(originalFsmPath,typeOfVisit,this.k); //pathsTmp
		
		//System.out.println(" a.size of sequences having max length k = "+pathsTmp.size());

		if (pathsTmp!=null) {
			for (Path path : pathsTmp) {
				pathTmp=new Path();
				pathTmp.copy(path);
				if (!exists(paths,pathTmp)) {
					paths.add(pathTmp);
				}
			}
		}
	
		selectPaths_OfLengthK(fsm);  //paths_K
		//System.out.println(" b.size of sequences having max length k = "+paths_K.size());

		System.out.println(" size of sequences having max length k ="+paths_K.size());
		selectSEMPaths_OfLengthK(fsm); //paths_K_sem
		
		System.out.println(" size of sequences of length k that are sem-semantic ="+paths_K_sem.size());
		
		if (reset) wp.printPaths(paths_K_sem,outPathFolder,fileName);
		
		return paths_K_sem;
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
		
		selectSEMPaths_OfLengthK(fsm);

		return getCopyOf_paths_K_sem();
	}
	
	Vector<Path> getCopyOf_paths_K_sem(){
		Vector<Path> copy_paths_K_sem=new Vector<Path>();
		Path p;
		for (Path path : paths_K_sem) {
			p=new Path();
			p.copy(path);
			if (!exists(copy_paths_K_sem,p)) {
				copy_paths_K_sem.add(p);
			}
		}
		return copy_paths_K_sem;
	}
	
	protected void selectSEMPaths_OfLengthK(FSM fsm){

		Path path_copy=null;
		Edge e1;
		Edge e2;
		Node s0=null;
		Node s1=null;	
		boolean issem=false;

		for (Path path : paths_K) {

			if (path.getEdges().size()==2) {

				e1=path.getEdges().get(0);
				e2=path.getEdges().get(1);
				s0=fsm.getStartNode();
				s1=path.getEdges().get(1).getTarget();

				issem=SEMchecker.isSEM(fsm, e1, e2, s0, s1);

			} else {
				for (int index = 1; index <  path.getEdges().size()-1; index++) {
					issem=false;

					e1=path.getEdges().get(index);
					e2=path.getEdges().get(index+1);
					s0=path.getEdges().get(index-1).getTarget();
					s1=path.getEdges().get(index+1).getTarget();


					if ((e1==null)||(e2==null)||(s0==null)||(s1==null)){
						issem=false;
					}

					issem=SEMchecker.isSEM(fsm, e1, e2, s0, s1);
					if (!issem){
						//issem=true;
						break;
					}

				}
			}


			if (issem){
				if (!exists(paths_K_sem,path)) {
					path_copy=new Path();
					path_copy.copy(path);
					paths_K_sem.add(path_copy);

				}
			}

		}

	}
	
}
