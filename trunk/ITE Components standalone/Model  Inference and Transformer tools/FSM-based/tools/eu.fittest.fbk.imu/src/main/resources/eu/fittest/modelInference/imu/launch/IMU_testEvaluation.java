package eu.fittest.modelInference.imu.launch;
import eu.fittest.modelInference.imu.config.Config_Launcher;
import eu.fittest.modelInference.imu.config.LoadConfiguration;
import eu.fittest.modelInference.imu.launch.imuProxy.IMUproxy_ForTestEvaluation;

/**
 * Test cases analysis -> ITSU (i.e., test of the alg3?). 
 * It works with files to test 'X' on a test set (in term of model and test cases). 
 * It requires 'X' (to set on Config_Launcher).
 * It 
 * 
 * @author Alessandro Marchetto
 *
 */
public class IMU_testEvaluation {
	IMUproxy_ForTestEvaluation imu;
	Config_Launcher cl=Config_Launcher.getInstace();
	LoadConfiguration predefinedConfig=new LoadConfiguration();
	
	public static void main(String[] args) {
		IMU_testEvaluation check=new IMU_testEvaluation();
		check.load();
		check.checkLogsByTest();
	}  
	
	public void load(){
		if (!predefinedConfig.loadConfiguration()){
			System.out.println("Configuration NOT loaded from XML ... the default one is used!");
		}
	}
	
	
	public void checkLogsByTest(){
			
		int X=cl.X;  //cambiare con output di calibration	.. customize it
		
		int maxFilePemutations=cl.maxFilePemutations;	
		int maxloop=cl.maxloop;
		String folderPath=cl.getFolderPathXNewLogsCheck(1);
		String fsm2dotFileName_prefix=cl.getFsm2dotFileName_prefix();
		String outputDirName=cl.getOutputDirName();

		imu=new IMUproxy_ForTestEvaluation(); 
		imu.setUpParams(X,maxFilePemutations,folderPath,fsm2dotFileName_prefix,outputDirName,true,maxloop,true);
		
		for (int i = 0; i < maxFilePemutations; i++) {
					
			imu.incrementalModelInferenceOnASetOfLogs_usedForCheckNewLogsByTest(i);
		
			int[] ops=imu.getNumOfOps();
			System.out.println(" Num Add ops="+ops[0]+" ; Num Rem ops="+ops[1]);
			
			int[] opsTS=imu.getNumOfOpsTS();
			System.out.println(" Num AddTC ops="+opsTS[0]+" ; Num KeepTS ops="+opsTS[1]+" ; Num DelTS ops="+opsTS[2]);
		}
		
	}
	
}
