package eu.fittest.modelInference.imu.launch;

import java.util.Vector;

import  eu.fittest.modelInference.fsmInference.manager.FSMmanager;
import eu.fittest.modelInference.fsmInference.fsm.Transition;

import eu.fittest.modelInference.imu.modelAlgs.PathExtraction;
import eu.fittest.modelInference.imu.test.ConvertPathToTeseCase;
import eu.fittest.modelInference.imu.test.TestCase;
import eu.fittest.modelInference.imu.modelAlgs.Path;

import eu.fittest.modelInference.imu.test.ContentVerification;

/**
 * IMU (alg.2?)
 * 
 * @author Alessandro Marchetto
 *
 */
public class ITSU {
	PathExtraction pathExt=new PathExtraction();
	
	Vector<TestCase> delTestCases;
	Vector<TestCase> keepTestCases;
	Vector<TestCase> addTestCases;
	
	public void resetCounter(){
		 delTestCases=new Vector<TestCase>();
		 keepTestCases=new Vector<TestCase>();
		 addTestCases=new Vector<TestCase>();
	}
	
	public boolean contained(Vector<TestCase> tcs, TestCase tc){
		for (int i = 0; i < tcs.size(); i++) {
			if (tcs.get(i).isEqual(tc)) return true;
		}
		return false;
	}
	
	public void incrementalTestSuiteUpdate(FSMmanager fsmmanager_new, Vector<TestCase> testsuite_Old, int maxloop, boolean resetCounter){
		
		
		 if (resetCounter) resetCounter();
		
		 
		 TestCase tc;
		 Transition event;
		 boolean del=false;
		
		
			 // DEL unused test cases
			 for (int tindex = 0; tindex < testsuite_Old.size(); tindex++) {
				 tc=testsuite_Old.get(tindex);
	
				 for (int eindex = 0; eindex < tc.size(); eindex++) {
						event=tc.getEventInTC(eindex);
						if (!ContentVerification.isEventContainedInFsm(fsmmanager_new.fsmAllInOne.transitions, event)){
							if (contained(delTestCases,tc)==false) {
								delTestCases.add(tc);
							}
							del=true;
						}
					}
				 
				 if (del==false) {
					 if (contained(keepTestCases,tc)==false) keepTestCases.add(tc);
				 }
				 
				 del=false; 
			 }
		
		 
		 
			 // ADD for uncovered target
			 PathExtraction pathExtFromTo=new PathExtraction();
			 long[] rootend=pathExtFromTo.setParams(fsmmanager_new,1);
			 TestCase tctmp;
			 Path p;
			 
			 Vector<Path> pathsFound=getPathsFromSourceToTarget(fsmmanager_new,maxloop);
			 
			 for (int eindex = 0; eindex < fsmmanager_new.fsmAllInOne.transitions.size(); eindex++) {
				 event= fsmmanager_new.fsmAllInOne.transitions.getTransitions().get(eindex);
				 
				 if (!ContentVerification.isEventContainedInTCS(testsuite_Old, event)){
					 if ((rootend[0]>-1)&&(event.getidTransition()>-1)) {
											 
						 if (pathsFound!=null){
							 if (pathsFound.size()>0){
								 p=getPathWithTarget(pathsFound,event.getidTransition());
								 if (p!=null){
									 tctmp=ConvertPathToTeseCase.convertP2TC(p, fsmmanager_new.fsmAllInOne.transitions);
									 if (contained(addTestCases,tctmp)==false) {
											 addTestCases.add(tctmp);
											 //System.out.println("added");
									 }
								 }	
								 else {
									// System.out.println("...not found tc to be added");
								 }
							 }
						 }
					 }
				 }				 		 
			
		 }
	}
	
	private Vector<Path> getPathsFromSourceToTarget(FSMmanager fsmmanager,int maxloop){
		 pathExt.pathExtraction(fsmmanager,maxloop);
		 Vector<Path> paths= pathExt.getExtractedPaths();
		 return paths;
	}
	private Path getPathWithTarget(Vector<Path> paths,long idTransition){
		 Path p=null;
		 Vector<Long> v=null;;
		 
		 for (int i = 0; i < paths.size(); i++) {
			 p=paths.get(i);
			 if (p.iscontainedTransition(new Long(idTransition))) return p;			 
		 }
		 return null;
	}
	

	public Vector<TestCase> getNewSuite(){
		Vector<TestCase> newSuite=new Vector<TestCase>();
		 for (int i = 0; i < keepTestCases.size(); i++) {
			 if (contained(newSuite,keepTestCases.get(i))==false) newSuite.add(keepTestCases.get(i));
		 }
		 for (int i = 0; i < addTestCases.size(); i++) {
			 newSuite.add(addTestCases.get(i));
			 if (contained(newSuite,addTestCases.get(i))==false) newSuite.add(addTestCases.get(i));
		 }
		 return newSuite;
	}
	
	
	public Vector<TestCase> extract_TCS_DFS(FSMmanager fsmmanager,int maxloop){
		 Vector<TestCase> tcs=new Vector<TestCase>();
		 TestCase tctmp;
		 
		 pathExt.pathExtraction(fsmmanager,maxloop);
		 Vector<Path> paths= pathExt.getExtractedPaths();
		 
		 for (int i = 0; i < paths.size(); i++) {
			 tctmp=ConvertPathToTeseCase.convertP2TC(paths.get(i), fsmmanager.fsmAllInOne.transitions);
			 if (contained(tcs,tctmp)==false) tcs.add(tctmp);
		}
		
		 return tcs;
	 }




	public int getAddTestCasesSize() {
		return addTestCases.size();
	}
	public int getDelTestCasesSize() {
		return delTestCases.size();
	}
	public int getKeepTestCasesSize() {
		return keepTestCases.size();
	}
	
}
