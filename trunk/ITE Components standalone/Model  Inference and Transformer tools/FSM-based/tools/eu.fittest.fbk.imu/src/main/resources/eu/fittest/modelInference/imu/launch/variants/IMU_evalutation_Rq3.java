package eu.fittest.modelInference.imu.launch.variants;

import eu.fittest.modelInference.fsmInference.manager.FSMmanager;
import eu.fittest.modelInference.imu.config.Config_Launcher;
import eu.fittest.modelInference.imu.config.LoadConfiguration;
import eu.fittest.modelInference.imu.launch.IMU_checkNewLogs;

import java.util.Vector;
import java.io.File;

/**
 * Evaluation for Rq3
 * 
 * @author Alessandro Marchetto
 *
 */
public class IMU_evalutation_Rq3 {

	LoadConfiguration predefinedConfig=new LoadConfiguration();
	Config_Launcher cl=Config_Launcher.getInstace();
	IMU_evalutation_Rq3 evaluationRq3;
	
	public static void main(String[] args) {
		IMU_evalutation_Rq3 evaluationRq3=new IMU_evalutation_Rq3();
		evaluationRq3.run();
	}
	
	public void run(){
		
		if (!predefinedConfig.loadConfiguration()){
			System.out.println("Configuration NOT loaded from XML ... the default one is used!");
			evaluationRq3=new IMU_evalutation_Rq3();
			evaluationRq3.repeat();
		}else {
			//System.out.println("\nIteration,X,nDel,nAdd,nCheck"); 
			repeat();
		}
	}
	
  public void repeat(){
		System.out.println("********* IMU");
		IMU_checkNewLogs newLogs=new IMU_checkNewLogs();
		
		Vector<int[]> vops;
		for (int j = 0; j < cl.XforRQ3.length; j++) {
			cl.X=cl.XforRQ3[j];
			vops=newLogs.checkLogs();
			for (int[] ops : vops) {
				System.out.println("Num Add ops="+ops[0]+" ; Num Rem ops="+ops[1]+" ; Num Check ops="+ops[2]+" ; X="+ops[3]);
			 	}
		}
		
		
		System.out.println("********* Model inference");
		
		FSMmanager fsmmanager=new FSMmanager();
		File[] filelist=fsmmanager.getFilelist(cl.folderPathRq3, cl.maxFilePemutations);
		
		
		int T=0;
		int adds=0;
		int dels=0;
		int checks=0;
		int processed_eventsInLogs=0;
		int machines=0;
		int logIndexLog=0;
			
		int[] ret;
		
		for (int j = 0; j < cl.TforRQ3.length; j++) {
		
			T=cl.TforRQ3[j];
			
			adds=0;
			dels=0;
			checks=0;
			processed_eventsInLogs=0;
			machines=0;
			logIndexLog=0;
				
			
			while (logIndexLog<filelist.length){
			
				ret=modelInference(logIndexLog,T);
				logIndexLog=ret[5];
				adds=adds+ret[0];
				dels=dels+ret[1];
				checks=checks+ret[2];
				processed_eventsInLogs=processed_eventsInLogs+ret[4];
				machines=machines+ret[3];
			
				//System.out.println("logIndexLog="+logIndexLog);
			}
		
			//System.out.println("T="+T+" FSM="+machines+" Num Add ops="+adds+" ; Num Rem ops="+dels+" ; Num Check ops="+checks+" ; processed_eventsInLogs="+processed_eventsInLogs);
			System.out.println("T="+T+" FSM="+machines+" Num Add ops="+adds+" ; Num Rem ops="+dels+" ; Num Check ops="+checks);
		}

		
	}
	
	
	public int[] modelInference(int startIndexLog,int stopIndexEvent){
		FSMmanager fsmmanager=new FSMmanager();
		File[] filelist=fsmmanager.getFilelist(cl.folderPathRq3, cl.maxFilePemutations);
	
		File[] fileCurrent=new File[1];

		int adds=0;
		int dels=0;
		int checks=0;
		int processed_eventsInLogs=0;
		int machines=0;
		int logIndex = startIndexLog;
		
		machines++;
		while ((logIndex < filelist.length)&&(processed_eventsInLogs<stopIndexEvent)){
			fileCurrent[0]=filelist[logIndex]; 
    		if (logIndex==0) fsmmanager.generateFSM(fileCurrent, true, 0);
    		else fsmmanager.generateFSM(fileCurrent, false, 0);
    		processed_eventsInLogs=fsmmanager.fsmAllInOne.eventsinlogs;
			logIndex++;
			adds=fsmmanager.fsmAllInOne.addops;
			dels=fsmmanager.fsmAllInOne.remops;
			checks=fsmmanager.fsmAllInOne.checks;
			
		}
		
		
		
		int[] ret=new int[6];
		ret[0]=adds;
		ret[1]=dels;
		ret[2]=checks;
		ret[3]=machines;
		ret[4]=processed_eventsInLogs;
		ret[5]=logIndex;
		
		return ret;
		

	}
	
}
