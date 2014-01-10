package eu.fittest.modelInference.imu.launch.imuProxy;

import eu.fittest.modelInference.fsmInference.manager.*;

import eu.fittest.modelInference.fsmInference.fsm.*;

import java.util.*;
import java.io.*;

import eu.fittest.modelInference.imu.utility.*;
import eu.fittest.modelInference.imu.test.TestCase;
import eu.fittest.modelInference.imu.launch.ITSU;
import eu.fittest.modelInference.imu.modelAlgs.*;

/**
 * 
 * 
 * @author Alessandro Marchetto
 *
 */
public class IMUproxy_ForTestEvaluation extends eu.fittest.modelInference.imu.launch.IMU {
	ITSU itsu;
	
	public IMUproxy_ForTestEvaluation(){
		super();
		itsu=new ITSU();
	}

	int[] ops=null;
	int[] opsTS=null;
	
	public void incrementalModelInferenceOnASetOfLogs_usedForCheckNewLogsByTest(int indexOfSequencetoUse){
		
		try{
			itsu=new ITSU();
			Vector<TestCase> suite1;
			Vector<TestCase> suite2;
			
			cleanFSM();
						
			Vector<File[]> listOfFilesSequences=logseqs.getSequenceList();
			File[] filelist=null;
			
			
			ops=new int[2];
			opsTS=new int[3];
			filelist=listOfFilesSequences.get(0); 
			File[] fileCurrent=new File[1];
			int currentTransIndex=0;
			int tmpTransIndex=0;
			
			int numeroFilesXModelConstruction=filelist.length-logseqs.numberOfCalibrationFiles;
					
			//uso solo le tracce di calibrazione
			for (int logIndex = 0; logIndex < filelist.length; logIndex++) {
				fileCurrent[0]=filelist[logIndex];  
				
				if (logIndex>numeroFilesXModelConstruction) {
    				tmpTransIndex=incrementalModelInferenceOnALog(fileCurrent,currentTransIndex, X, true, false, true);
    				currentTransIndex=tmpTransIndex;	
    			}	
			}
			
			//fix();
			fsmmanager.FSM2DOT(fsmmanager.fsmAllInOne,"modelConstructed_Test_"+fsm2dotFileName_final);
			suite1=itsu.extract_TCS_DFS(fsmmanager, 0);
			System.out.println("Size _initialModel_ = "+fsmmanager.fsmAllInOne.transitions.size()+" Number of TCs _initialSuite_ = "+suite1.size());
			
			
			fileCurrent=new File[1];
			currentTransIndex=0;
			tmpTransIndex=0;
				
			filelist=listOfFilesSequences.get(indexOfSequencetoUse); 
			resetOpsCounter();
			opsTS=new int[3];
			opsTS[0]=0;
			opsTS[1]=0;
			opsTS[2]=0;
			
	    	for (int logIndex = 0; logIndex < filelist.length; logIndex++) {
	    		fileCurrent[0]=filelist[logIndex];  
	    		
	    		//uso solo le tracce iniziali, applico remove, non retto index di esecuzione
	    		if (logIndex<=numeroFilesXModelConstruction) {

		    		tmpTransIndex=incrementalModelInferenceOnALog(fileCurrent, currentTransIndex, X, true, true, true);
		    		currentTransIndex=tmpTransIndex;
		    	
		    		if (logIndex==0)itsu.incrementalTestSuiteUpdate(fsmmanager, suite1, 0, true);
		    		else itsu.incrementalTestSuiteUpdate(fsmmanager, suite1, 0, false);
		    		
		    		suite2=itsu.getNewSuite();
		    		
		    		suite1=new Vector<TestCase>();
		    		
		    		for (int i = 0; i < suite2.size(); i++) {
		    			 suite1.add(suite2.get(i));
		    		 }
		    		
	    			}
	    	}
	    	
	    	//fix(); //non necessaria.. al limite fare per sicurezza		
	    	fsmmanager.FSM2DOT(fsmmanager.fsmAllInOne,"modelUpdated_Test_"+fsm2dotFileName_final);
			System.out.println("Size _finalModel_ = "+fsmmanager.fsmAllInOne.transitions.size()+" Number of TCs _finalSuite_ = "+suite1.size());
			
			opsTS[0]=itsu.getAddTestCasesSize();
			opsTS[1]=itsu.getKeepTestCasesSize();
			opsTS[2]=itsu.getDelTestCasesSize();

			ops[0]=getNumOfAddOps();
			ops[1]=getNumOfRemOps();
			
			
			
		}catch(Exception e){
			System.out.println("Error in: modelsInference");
			e.printStackTrace();
		}

		
	}
	
	public void resetOpsCounter(){
		fsmmanager.fsmAllInOne.resetOpsCounter();
	}
	public int[] getNumOfOps(){
		return ops;
	}
	public int[] getNumOfOpsTS(){
		return opsTS;
	}
	protected int getNumOfAddOps(){
		return fsmmanager.fsmAllInOne.addops;
	}
	protected int getNumOfRemOps(){
		return fsmmanager.fsmAllInOne.remops;
	}
	

}
