package eu.fittest.modelInference.imu.launch.imuProxy;

import java.util.*;



import java.io.*;

import eu.fittest.modelInference.imu.config.Config_Launcher;
import eu.fittest.modelInference.imu.launch.IMU;
import eu.fittest.modelInference.imu.modelAlgs.*;
import eu.fittest.modelInference.fsmInference.fsm.*;

/**
 * 
 * 
 * @author Alessandro Marchetto
 *
 */
public class IMUproxy_ForCalibration extends eu.fittest.modelInference.imu.launch.IMU {
	
	Config_Launcher cl=Config_Launcher.getInstace();
	
	public IMUproxy_ForCalibration(){
		super();
	}
	
	public void setUPforTraces(boolean subdir,String folderPath){

		fsm2dotFileName_final=fsm2dotFileName_prefix+"_"+outputDirName+"_FSMfinal"; 

		logseqs.setUp(folderPath, maxFilePemutations, subdir);
	
		
	}
	
	File[] getFilelist(int indexOfSequencetoUse){
		Vector<File[]> listOfFilesSequences=logseqs.getSequenceList();
		
		File[] filelist=null;
			
		filelist=listOfFilesSequences.get(indexOfSequencetoUse);
		return filelist;
	}
	


	public int[] incrementalModelInferenceOnASetOfLogs_usedForCalibration(boolean cleanFSM, int remove,int indexOfSequencetoUse, boolean readLogsFromFiles, Vector<Path> suite,  Fsm fsm_initial){
	
		try{
			
			if (cleanFSM) cleanFSM();
		
			int x_tobeReturned=-1;
			
			File[] filelist=null;
			Path ptc;
			int currentTransIndex=0;
			int tmpTransIndex=0;
			int numeroFilesXModelConstruction=0;
			
			if (readLogsFromFiles) {
				filelist=getFilelist(indexOfSequencetoUse);
				numeroFilesXModelConstruction=filelist.length-logseqs.numberOfCalibrationFiles;
				
				File[] fileCurrent=new File[1];

		    	for (int logIndex = 0; logIndex < filelist.length; logIndex++) {
		    		fileCurrent[0]=filelist[logIndex];  
		    		
		    		if (remove==0){ //uso solo le tracce iniziali ma senza applicare la remove
		    			if (logIndex<numeroFilesXModelConstruction) {
		    				tmpTransIndex=incrementalModelInferenceOnALog(fileCurrent,currentTransIndex,X, true, false, true);
		    			}
		    		}
		    		else if (remove==1){ //uso solo le tracce iniziali e applico remove
		    			if (logIndex<numeroFilesXModelConstruction) {
		    				tmpTransIndex=incrementalModelInferenceOnALog(fileCurrent,currentTransIndex,X, true, true, true);
		    			}
		    		}
		    		else if (remove==2){ //uso tutte le tracce e non applico la remove
		    			tmpTransIndex=incrementalModelInferenceOnALog(fileCurrent,currentTransIndex,X, true, false, true);
		    			
		    		}
		    		else if (remove==3){  //uso solo le tracce della directory xCalibration
		    			if (logIndex>numeroFilesXModelConstruction) {
		    				tmpTransIndex=incrementalModelInferenceOnALog(fileCurrent,currentTransIndex,X, true, false, true);
		    			}
		    		}
		    		else if (remove==4){ //uso solo le tracce iniziali, applico remove, non ret index di esecuzione
			    			if (logIndex<numeroFilesXModelConstruction) {
			    				tmpTransIndex=incrementalModelInferenceOnALog(fileCurrent,currentTransIndex,X, true, true, true);
			    			}
			    	}
		    			
		    		//System.out.println(" logs ->"+fsmmanager.countEventsInLogs);
		    		
		    		currentTransIndex=tmpTransIndex;	
		    		x_tobeReturned=currentTransIndex-fsmmanager.fsmAllInOne.transitions.getLowerTransitionExecIndex();
		    		
		    	}
				
			}
			else {
				
			}
				    	
	    	//fix(); //non necessaria.. al limite fare per sicurezza		
	    	fsmmanager.FSM2DOT(fsmmanager.fsmAllInOne,fsm2dotFileName_final);
			
		
			//}
			int[] ret=new int[4];
			ret[0]=x_tobeReturned;
			ret[1]=fsmmanager.fsmAllInOne.transitions.size();
			ret[2]=numeroFilesXModelConstruction;
			ret[3]=logseqs.numberOfCalibrationFiles;
			return ret;
			
		}catch(Exception e){
			System.out.println("Error in: modelsInference");
			e.printStackTrace();
			return null;
		}

		
	}
	
	
}
