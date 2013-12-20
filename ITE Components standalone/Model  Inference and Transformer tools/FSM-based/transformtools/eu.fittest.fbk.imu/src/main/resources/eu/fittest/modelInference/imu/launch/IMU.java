package eu.fittest.modelInference.imu.launch;

import eu.fittest.modelInference.fsmInference.manager.*;
import eu.fittest.modelInference.fsmInference.fsm.*;
import eu.fittest.modelInference.imu.utility.*;

import java.util.*;
import java.io.*;

import eu.fittest.modelInference.imu.config.Config_Launcher;

import eu.fittest.modelInference.imu.modelAlgs.*; 

/**
 * IMU (alg.1?)
 * 
 * @author Alessandro Marchetto
 *
 */
public class IMU {
	Config_Launcher cl=Config_Launcher.getInstace();
	
	public LogSequences logseqs=LogSequences.getInstace();
	FSMmanager fsmmanagerSingleTrace;
	public FSMmanager fsmmanager;
	
	public String folderPath="input/esempio";
	Unique uniqueIds=Unique.getInstace();
	
	public String fsm2dotFileName_prefix="";
	protected String fsm2dotFileName_final=fsm2dotFileName_prefix+"_outFSM_final"; 
	public String outputDirName=""; 
	
	public int X=5;
	public int maxFilePemutations=1;
	public int maxloop=1;
	
	Utility utils=new Utility();
	Fix fixModel=new Fix();
	
 //da qui esegito per il settings
	
	public static void main(String[] args) {
		IMU p=new IMU();
		p.setUpParams();
		p.automaticSetUp(false,true);
		p.incrementalModelInferenceOnASetOfLogs();
	}

	public void setUpParams(){
		X=cl.getX();
		maxloop=cl.getMaxloop();
		maxFilePemutations=cl.getMaxFilePemutations();
		folderPath=cl.getFolderPath(); 
		fsm2dotFileName_prefix=cl.getFsm2dotFileName_prefix();
		outputDirName=cl.getOutputDirName();	
    }
	
	
	public void setUpParams(int X,int maxFilePemutations,String folderPath,String fsm2dotFileName_prefix,String outputDirName,boolean subdir, int maxloop,boolean tracesOnfiles){
				
		this.X=X;
		this.maxloop=maxloop;
		this.maxFilePemutations=maxFilePemutations;
		this.folderPath=folderPath; 
		this.fsm2dotFileName_prefix=fsm2dotFileName_prefix;
		this.outputDirName=outputDirName;
		//check
		automaticSetUp(subdir, tracesOnfiles);
		
			
    }
    
	
	public void automaticSetUp(boolean subdir,boolean tracesOnfiles){

		fsm2dotFileName_final=fsm2dotFileName_prefix+"_"+outputDirName+"_FSMfinal"; 

		if (tracesOnfiles) logseqs.setUp(folderPath, maxFilePemutations, subdir);
	
		
	}
	

	public int incrementalModelInferenceOnALog(File[] fileCurrent,int currentTransIndex, int x, boolean resetCurrentTransIndex, boolean remove, boolean resetFSM){
				
		if (resetCurrentTransIndex==false) {
			currentTransIndex=fsmmanager.fsmAllInOne.transitions.getHigherTransitionExecIndex();
		}
		fsmmanagerSingleTrace.generateFSM(fileCurrent, true, currentTransIndex);
		
		int tmpTransIndex=fsmmanager.addEdgeToFSM(fsmmanagerSingleTrace.fsmAllInOne,currentTransIndex,fsmmanagerSingleTrace.fsmAllInOne.eventsinlogs);
		
		if (remove) fsmmanager.removeEdges(currentTransIndex-x);
		 
		return tmpTransIndex;
	}
	
	
	
	public void cleanFSM(){
		uniqueIds.reset(); 
		fsmmanagerSingleTrace=new FSMmanager();
		fsmmanager=new FSMmanager();
	}
		
	
	public void fix(){
		
		fixModel.fixModel(fsmmanager);
		Vector<Long> toBeRemovedTId=fixModel.getTtoBeRemoved();
		Vector<Long> toBeRemovedSId=fixModel.getStoBeRemoved();		
		
		for (int i = 0; i < toBeRemovedTId.size(); i++) {
			fsmmanager.fsmAllInOne.removeTransition(toBeRemovedTId.get(i).longValue());
		}
		for (int i = 0; i < toBeRemovedSId.size(); i++) {
			fsmmanager.fsmAllInOne.removeState(toBeRemovedSId.get(i).longValue());
		}
	}
	
	
	public void incrementalModelInferenceOnASetOfLogs(){
		
		try{
			
			cleanFSM();
						
			Vector<File[]> listOfFilesSequences=logseqs.getSequenceList();
			File[] filelist=null;
			
			
			
			filelist=listOfFilesSequences.get(0); 
			File[] fileCurrent=new File[1];
			int currentTransIndex=0;
			int tmpTransIndex=0;
			
			int numeroFilesXModelConstruction=filelist.length-logseqs.numberOfCalibrationFiles;
					
			//uso solo le tracce di calibrazione
			for (int logIndex = 0; logIndex < filelist.length; logIndex++) {
				fileCurrent[0]=filelist[logIndex];  
				
				if (logIndex<numeroFilesXModelConstruction) {
    				tmpTransIndex=incrementalModelInferenceOnALog(fileCurrent,currentTransIndex, X, true, true, true);
    				currentTransIndex=tmpTransIndex;	
    			}	
			}
			
			//fix();
			fsmmanager.FSM2DOT(fsmmanager.fsmAllInOne,"modelConstructed&Updated_"+fsm2dotFileName_final);
			cl.setGenerateTraceFileName("modelConstructed&Updated_"+fsm2dotFileName_final);
					
						
		}catch(Exception e){
			System.out.println("Error in: modelsInference");
			e.printStackTrace();
		}

		
	}

}
