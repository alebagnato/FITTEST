package eu.fittest.modelInference.imu.launch;

import eu.fittest.modelInference.imu.config.Config_Launcher;
import eu.fittest.modelInference.imu.config.LoadConfiguration;

import java.util.Vector;
import eu.fittest.modelInference.imu.launch.imuProxy.IMUproxy_ForCheckNewLogs;

/**
 * Check a new set of logs. It works with files. 
 * It requires 'X' (expiration time), to set on Config_Launcher.
 * It turns back the number of add and del operations. 
 * 
 * @author Alessandro Marchetto
 *
 */
public class IMU_checkNewLogs {
	IMUproxy_ForCheckNewLogs imu;
	Config_Launcher cl=Config_Launcher.getInstace();
	LoadConfiguration predefinedConfig=new LoadConfiguration();
	
	public static void main(String[] args) {
		IMU_checkNewLogs check=new IMU_checkNewLogs();
		check.load();
		check.checkLogs();
	}  
	
	public void load(){
		if (!predefinedConfig.loadConfiguration()){
			System.out.println("Configuration NOT loaded from XML ... the default one is used!");
		}
	}
	
	
	public Vector<int[]> checkLogs(){
		
		Vector<int[]> v=new Vector<int[]> ();
		
		int X=cl.X;  //cambiare con output di calibration
		//int X=266; 

		int maxFilePemutations=cl.maxFilePemutations;	
		int maxloop=cl.maxloop;
		//String folderPath=cl.getFolderPathXNewLogsCheck(1);
		String folderPath=cl.folderPathRq3;
		String fsm2dotFileName_prefix=cl.getFsm2dotFileName_prefix();
		String outputDirName=cl.getOutputDirName();

		imu=new IMUproxy_ForCheckNewLogs(); 
		imu.setUpParams(X,maxFilePemutations,folderPath,fsm2dotFileName_prefix,outputDirName,true,maxloop,true);
		
		for (int i = 0; i < maxFilePemutations; i++) {
				
			imu.incrementalModelInferenceOnASetOfLogs_usedForCheckNewLogs(i,true, null,null);
		
			int[] ops=imu.getNumOfOps();
			//System.out.println(" Num Add ops="+ops[0]+" ; Num Rem ops="+ops[1]+" ; Num Check ops="+ops[2]);
			v.add(ops.clone());
			
		}
		
		return v;
		
	}
	
	
	public void checkLogs(int X,int maxFilePemutations,String folderPath,int maxloop){
		
		String fsm2dotFileName_prefix=cl.getFsm2dotFileName_prefix();
		String outputDirName=cl.getOutputDirName();

		imu=new IMUproxy_ForCheckNewLogs(); 
		imu.setUpParams(X,maxFilePemutations,folderPath,fsm2dotFileName_prefix,outputDirName,true,maxloop,true);
		
		for (int i = 0; i < maxFilePemutations; i++) {
				
			imu.incrementalModelInferenceOnASetOfLogs_usedForCheckNewLogs(i,true, null,null);
		
			int[] ops=imu.getNumOfOps();
			System.out.println(" Model Update===> Num Add ops="+ops[0]+" ; Num Rem ops="+ops[1]);
			
		}
		
	}
	
}
