package eu.fittest.eventSequenceGenerator.randomCoverage;

import java.util.*;


import eu.fittest.eventSequenceGenerator.utility.*;

import eu.fittest.modelInference.imu.test.ConvertPathToTeseCase;
import eu.fittest.modelInference.imu.test.TestCase;
import eu.fittest.modelInference.imu.modelAlgs.Path;
import eu.fittest.modelInference.imu.modelAlgs.Probabilistic_PathExtraction;
import eu.fittest.modelInference.imu.config.Config_Launcher;
import eu.fittest.modelInference.imu.utility.Utility;

import eu.fittest.modelInference.fsmInference.fsm.Fsm;
import eu.fittest.modelInference.fsmInference.fsm.Transition;

/**
 * 
 * 
 * @author Alessandro Marchetto
 *
 */
public class PathsGenerator {
	Probabilistic_PathExtraction prob_pathExt=new Probabilistic_PathExtraction();
	Hashtable<Long,Double> event_probability;
	int maxLength_SinglePath;
	WriteTrace wIt=new WriteTrace();
	Config_Launcher cl=Config_Launcher.getInstace();
	Utility utils=new Utility();
	Vector<TestCase> suite=null;
	Vector<Path> paths;
		
	public Vector<eu.fittest.eventSequenceGenerator.data.Path> generateTraces_withProbabilities(Hashtable<Long,Double> event_probability,Fsm fsm, int maxLength_SinglePath,String dirOutput,String fileName,String setTypeOfCoverage){
	
		this.event_probability=event_probability;
		this.maxLength_SinglePath=maxLength_SinglePath;
							
		suite=null;
		
		if (!utils.createDirectory(dirOutput)){
			utils.emptyDirectory(dirOutput);
		}
				
		suite=extract_TCS_DFS_withProbability(fsm,setTypeOfCoverage);
		wIt.writeInFiles(suite,dirOutput,fileName,fsm);
		
		
		Vector<eu.fittest.eventSequenceGenerator.data.Path> newPaths=new Vector<eu.fittest.eventSequenceGenerator.data.Path>();
		for (int i = 0; i < paths.size(); i++) {
			newPaths.add(convertPathFormat(paths.get(i),fsm));		
		}		
		return newPaths;
	}
	
	public eu.fittest.eventSequenceGenerator.data.Path convertPathFormat(Path pathOld,Fsm fsm){
		eu.fittest.eventSequenceGenerator.data.Path pathNew=new eu.fittest.eventSequenceGenerator.data.Path();
		
		//int id=pathOld.getId();
		//Vector<Long> states=pathOld.getPathVector();
		//Vector<Long> edges=pathOld.getPathTransitionsVector();
		
		Transition t;
		eu.fittest.eventSequenceGenerator.data.Edge e;
		eu.fittest.eventSequenceGenerator.data.Node n;
		
		for (int i = 0; i < pathOld.size(); i++) {
		
				t=fsm.transitions.getTransitionById(pathOld.getPathTransitionsVector().get(i));
				//attenzione allo stato
				n=new eu.fittest.eventSequenceGenerator.data.Node(fsm.states.getStateById(t.getIdStateTarget()).getStateContent()[0]);
				
				pathNew.add(new eu.fittest.eventSequenceGenerator.data.Edge(n,t.getTransitionContent()[0]));
	
		}
		 
		return pathNew;
	}
	
	private Vector<TestCase> extract_TCS_DFS_withProbability(Fsm fsm,String setTypeOfCoverage){
		 Vector<TestCase> tcs=new Vector<TestCase>();
		 TestCase tctmp;
		 
		 prob_pathExt=new Probabilistic_PathExtraction(); 
		 
		 if ((setTypeOfCoverage.startsWith("coverage"))) prob_pathExt.setTypeOfCoverage("coverage");
		 else prob_pathExt.setTypeOfCoverage(setTypeOfCoverage);
		 
		 //System.out.println("sizeFSM 1="+fsm.getEdgesInConvertedFormat().size());
		 
		 paths=prob_pathExt.pathsExtraction_withProbability(fsm,this.event_probability,maxLength_SinglePath,1);
		 
		 //System.out.println("sizeTCS 2="+paths.size());
		
		 for (int i = 0; i < paths.size(); i++) {
			 
			tctmp=ConvertPathToTeseCase.convertP2TC(paths.get(i), fsm.transitions);
			//if (tctmp.size()>2) {
			if (!isContained(tcs,tctmp)) {
				tcs.add(tctmp);
			}
			//} 
			 
 
		}
		
		 return tcs;
	 }

	
	boolean isContained(Vector<TestCase> tcs, TestCase t){
		
		for (TestCase test : tcs) {
			if (test.isEqual(t)) return true;
		}
		return false;
	}
	
}