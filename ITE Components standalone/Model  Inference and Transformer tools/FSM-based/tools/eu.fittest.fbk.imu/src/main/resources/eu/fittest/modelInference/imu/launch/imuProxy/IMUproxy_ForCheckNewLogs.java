package eu.fittest.modelInference.imu.launch.imuProxy;


import java.util.*;


import eu.fittest.modelInference.fsmInference.fsm.*;
import java.io.*;

import eu.fittest.modelInference.imu.config.Config_Launcher;
import eu.fittest.modelInference.imu.modelAlgs.Path;

/**
 * 
 * 
 * @author Alessandro Marchetto
 *
 */
public class IMUproxy_ForCheckNewLogs extends eu.fittest.modelInference.imu.launch.IMU {
	
	Config_Launcher cl=Config_Launcher.getInstace();
	
	public IMUproxy_ForCheckNewLogs(){
		super();
	}

	int[] ops=null;
	
	public void incrementalModelInferenceOnASetOfLogs_usedForCheckNewLogs(int indexOfSequencetoUse, boolean readLogsFromFiles, Vector<Path> suite, Fsm fsm_initial){
		
		try{
			
			cleanFSM();
			
			ops=new int[5];
			int currentTransIndex=0;
			int tmpTransIndex=0;
			int numeroFilesXModelConstruction=0;
			
			Path ptc;
			
			if (readLogsFromFiles) {
						
				Vector<File[]> listOfFilesSequences=logseqs.getSequenceList();
				File[] filelist=null;
				
				filelist=listOfFilesSequences.get(0); 
				File[] fileCurrent=new File[1];
				
				numeroFilesXModelConstruction=filelist.length-logseqs.numberOfCalibrationFiles;
						
				//uso solo le tracce di calibrazione
				for (int logIndex = 0; logIndex < filelist.length; logIndex++) {
					fileCurrent[0]=filelist[logIndex];  
					
					if (logIndex>=numeroFilesXModelConstruction) {
	    				tmpTransIndex=incrementalModelInferenceOnALog(fileCurrent,currentTransIndex, X, true, false, true);
	    				currentTransIndex=tmpTransIndex;	
	    			}	
				}
				
				//fix();
				fsmmanager.FSM2DOT(fsmmanager.fsmAllInOne,"modelConstructed_"+fsm2dotFileName_final);
				
				
				fileCurrent=new File[1];
				currentTransIndex=0;
				tmpTransIndex=0;
				fsmmanager.fsmAllInOne.addops=0;
				fsmmanager.fsmAllInOne.remops=0;
				fsmmanager.fsmAllInOne.checks=0;
									
				filelist=listOfFilesSequences.get(indexOfSequencetoUse); 
				resetOpsCounter();
				
		    	for (int logIndex = 0; logIndex < filelist.length; logIndex++) {
		    		fileCurrent[0]=filelist[logIndex];  
		    		
		    		//uso solo le tracce iniziali, applico remove, non ret index di esecuzione
		    		if (logIndex<numeroFilesXModelConstruction) {
			    			tmpTransIndex=incrementalModelInferenceOnALog(fileCurrent,currentTransIndex,X, true, true, true);
			    			currentTransIndex=tmpTransIndex;	
			    		}
		    	}
		    	
		    	//fix(); //non necessaria.. al limite fare per sicurezza		
		    	fsmmanager.FSM2DOT(fsmmanager.fsmAllInOne,"modelUpdated_"+fsm2dotFileName_final);
	
				ops[0]=getNumOfAddOps();
				ops[1]=getNumOfRemOps();
				ops[2]=getNumOfCheckOps();
				ops[3]=X;
				ops[4]=getFSMsize();
						
		}else{
		
		}
			
			
			
		}catch(Exception e){
			System.out.println("Error in: modelsInference");
			e.printStackTrace();
		}

		
	}
	
	
	public void resetOpsCounter(){
		fsmmanager.fsmAllInOne.resetOpsCounter();
	}
	public int getFSMsize(){
		return fsmmanager.fsmAllInOne.transitions.size();
	}
	public int[] getNumOfOps(){
		return ops;
	}
	protected int getNumOfAddOps(){
		return fsmmanager.fsmAllInOne.addops;
	}
	protected int getNumOfRemOps(){
		return fsmmanager.fsmAllInOne.remops;
	}
	protected int getNumOfCheckOps(){
		return fsmmanager.fsmAllInOne.checks;
	}
	
	
}
