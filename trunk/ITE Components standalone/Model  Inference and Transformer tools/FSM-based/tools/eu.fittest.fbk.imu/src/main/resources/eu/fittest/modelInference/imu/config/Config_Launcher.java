package eu.fittest.modelInference.imu.config;

import java.util.Vector;
/**
 * Configuration file for the whole IMU project
 * 
 * @author Alessandro Marchetto
 *
 */
public class Config_Launcher {
	
	private static Config_Launcher instance = null;
	 
    private Config_Launcher() {}
 
    public static Config_Launcher getInstace() {
        if (instance == null) {
        	instance = new Config_Launcher();
        }
        return instance;
    }
    
    public String configurationFileName="input"+System.getProperty("file.separator")+"configuration.properties";
    
    //Percentage of TCS for T1, T2 and Ttest (at log files level - not at event level)
    public double t1_PercOfExpectedLogs=0.25;
    public double t2_PercOfExpectedLogs=0.40;
    public double ttest_PercOfExpectedLogs=0.35;

    
    //public int[] probabilitiesCoefficientArray = {10, 100, 1, 50, 500, 1000, 5000, 10000, 1000000, 1005000, 1500000, 10000000, 100000000, 1000000000};
    
    //corrispondenza {100% 90% 80% 70% 60% 50% 40% 30% 20% 10%}
    //public double[] probabilitiesCoefficientArray = {1.0, 1.1, 1.25, 1.4, 1.6, 2.0, 2.5, 3.3, 5.0, 10.0};
    //public double[] probabilitiesCoefficientArray = {1.0, 1.1, 1.25, 1.4, 1.6, 2.0, 2.5, 3.3, 5.0, 10.0};
    
    //public double[] probabilitiesCoefficientArray = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 10.0};
    //public double[] probabilitiesCoefficientArray = {1.0, 10.0, 60.0, 110.0, 210.0, 310.0, 510.0, 710.0, 1000.0};
    //public double[] probabilitiesCoefficientArray = {1.0, 10.0, 30.0, 50.0, 70.0, 90.0, 100.0, 210.0, 310.0, 510.0, 710.0, 1000.0};

    public double[] probabilitiesCoefficientArray = {1.0, 6.0, 12.0, 18.0, 24.0, 32.0, 38.0, 44.0, 50.0};
    
    //public double[] probabilitiesCoefficientArray = {1.0, 1.5, 2.8, 5.0, 30.0, 200.0};
    //public double[] probabilitiesCoefficientArray = {1.0, 1000.0};
    //public double[] probabilitiesCoefficientArray = {1.0, 2.0};
    
    //corrispondenza {100% 95% 90% 85 80% 75% 70% 65 60% 55 50%}
    //public double[] probabilitiesCoefficientArray = {1.0, 1.05, 1.1, 1.17, 1.25, 1.33, 1.42, 1.53, 1.66, 1.81, 2};
    //public double[] probabilitiesCoefficientArray = {1.0, 1.42, 1.05, 1.1, 1.17, 1.25, 1.33, 1.42, 1.53, 1.66, 1.81, 2};
    
    //public int[] probabilitiesCoefficientArray = {1, 50, 100, 1, 500, 1000, 5000, 10000, 15000, 1000000, 1005000, 1500000, 10000000};
    //public int[] probabilitiesCoefficientArray = {1, 10, 10, 10, 100, 1000, 10000, 1000000, 10000000, 100000000, 1000000000};
    //public int[] probabilitiesCoefficientArray = {1, 2000000000, 1000000000, 100000000, 10000000, 1000000, 10000, 1000, 100, 10, 1};
    
    //evaluation A
    public int evaluationA_iterations_perEvent=5;
    //public int evaluationA_max=1000;
    public int evaluationA_fsmcoverageTimes_PerEvent=20;
    //public int evaluationA_iterations=10;
    //public int evaluationA_min_numberOfTracesForCalibation=2000; //per avere test a 300
    public int evaluationA_maxLengthOfGeneratedTests=20;
   /* 
    * mettere evaluationA_percent4xfixed= "-1" se si vuole usare "x_tobeReturned" oppure, e.g., a "0.2" che significa "20%"
    */
    //public double evaluationA_percent4xfixed=0.1;  //per avere FIX al 10%
    //public double evaluationA_percent4xfixed=0.2;  //per avere FIX al 20%
    public double evaluationA_percent4xfixed=-1.0; //per avere NOt-FIX
    
//---------    
    /*
     * mettere il numero di step a cui si vuole selezionare l'evento da analizzare
     */
    public int evaluationA_distanceAtWhichSelectEvent=10; //per avere VICINO
    //public int evaluationA_distanceAtWhichSelectEvent=12; //per avere LONTANO

    public int numberOfEventsToCheckRandomly=5;
    
    public Vector<String[]> eventsTocheck; //---vector<stateBefore, event, stateAfter>
//--------- only for RQ3
    
    public int[] TforRQ3 = {266, 700};
    public int[] XforRQ3 = {266, 700};
//---------    
    
    public void selectCurrentConf_percent4xfixed(int index){
    	 /* 	if (index==1){
    		evaluationA_percent4xfixed=0.5;
    		evaluationA_distanceAtWhichSelectEvent=6;
    	}*/
    	
   
    	if (index==1){
    		evaluationA_percent4xfixed=-1.0;
    		//evaluationA_percent4xfixed=0.2;
    		evaluationA_distanceAtWhichSelectEvent=6;
    	}/*else if (index==2){
    		evaluationA_percent4xfixed=0.1;
    		evaluationA_distanceAtWhichSelectEvent=6;
    	}
    	else if (index==3){
    		evaluationA_percent4xfixed=0.2;
    		evaluationA_distanceAtWhichSelectEvent=2;
    	}else if (index==4){
    		evaluationA_percent4xfixed=0.3;
    		evaluationA_distanceAtWhichSelectEvent=7;
    	}
    	*/
    	
    }
	
    //algo params
    public double alpha=1;
	public int X=2;
	public int maxFilePemutations=1;
	public int maxloop=2;
	
	//public boolean change=true;
	//
	
//X ESEMPIO
	public String folderPath="input"+System.getProperty("file.separator")+"calibration2";
	public String folderPathRq3="input"+System.getProperty("file.separator")+"calibration";
	
	public String folderPathAutomaticCalibration="input"+System.getProperty("file.separator")+"auto_calib";
	public String folderPathAutomaticCalibration_subDir="input"+System.getProperty("file.separator")+"auto_calib"+System.getProperty("file.separator")+"xCalibration";
	public String folderPathAutomaticCalibration_testDir="input"+System.getProperty("file.separator")+"auto_calib_test";
	
	//for testing
	public String folderPathXNewLogsCheck1="input"+System.getProperty("file.separator")+"testing-exp1"; 
	public String folderPathXNewLogsCheck2="input"+System.getProperty("file.separator")+"testing-exp2-conOld"; 
	public String folderPathXNewLogsCheck3="input"+System.getProperty("file.separator")+"testing-exp2-conOld-b"; 
	
	//public String eventName_fordensitycomputation="click_menuToMe";
	
	public String fsm2dotFileName_prefix="tudu";

	/*
 //X TUDU
	public String folderPath="input"+System.getProperty("file.separator")+"calibration2";
	
	public String folderPathAutomaticCalibration="input"+System.getProperty("file.separator")+"auto_calib";
	public String folderPathAutomaticCalibration_subDir="input"+System.getProperty("file.separator")+"auto_calib"+System.getProperty("file.separator")+"xCalibration";
	public String folderPathAutomaticCalibration_testDir="input"+System.getProperty("file.separator")+"auto_calib_test";
	
	//for testing
	public String folderPathXNewLogsCheck1="input"+System.getProperty("file.separator")+"testing-exp1"; 
	public String folderPathXNewLogsCheck2="input"+System.getProperty("file.separator")+"testing-exp2-conOld"; 
	public String folderPathXNewLogsCheck3="input"+System.getProperty("file.separator")+"testing-exp2-conOld-b"; 
	
	public String eventName_fordensitycomputation="click_menuToMe";
	
	public String fsm2dotFileName_prefix="tudu61";
*/
/*
	// x ORYX
    //public String folderPath="input"+System.getProperty("file.separator")+"oryx"+System.getProperty("file.separator")+"calibration2";
    public String folderPath="input"+System.getProperty("file.separator")+"oryx"+System.getProperty("file.separator")+"calibration2_s3";
	
	public String folderPathAutomaticCalibration="input"+System.getProperty("file.separator")+"oryx"+System.getProperty("file.separator")+"auto_calib";
	public String folderPathAutomaticCalibration_subDir="input"+System.getProperty("file.separator")+"oryx"+System.getProperty("file.separator")+"auto_calib"+System.getProperty("file.separator")+"xCalibration";
	public String folderPathAutomaticCalibration_testDir="input"+System.getProperty("file.separator")+"oryx"+System.getProperty("file.separator")+"auto_calib_test";
	
	//for testing
	public String folderPathXNewLogsCheck1="input"+System.getProperty("file.separator")+"oryx"+System.getProperty("file.separator")+"testing-exp1"; 
	public String folderPathXNewLogsCheck2="input"+System.getProperty("file.separator")+"oryx"+System.getProperty("file.separator")+"testing-exp2-conOld"; 
	public String folderPathXNewLogsCheck3="input"+System.getProperty("file.separator")+"oryx"+System.getProperty("file.separator")+"testing-exp2-conOld-b"; 
	
	public String eventName_fordensitycomputation="click_extdd-260";
	
	public String fsm2dotFileName_prefix="orxy15";
*/


/*	//x PAFM
    //public String folderPath="input"+System.getProperty("file.separator")+"pafm"+System.getProperty("file.separator")+"calibration2";
	//public String folderPath="input"+System.getProperty("file.separator")+"pafm"+System.getProperty("file.separator")+"calibration2_s";
	public String folderPath="input"+System.getProperty("file.separator")+"pafm"+System.getProperty("file.separator")+"calibration2_s_2";
	
	public String folderPathAutomaticCalibration="input"+System.getProperty("file.separator")+"pafm"+System.getProperty("file.separator")+"auto_calib";
	public String folderPathAutomaticCalibration_subDir="input"+System.getProperty("file.separator")+"pafm"+System.getProperty("file.separator")+"auto_calib"+System.getProperty("file.separator")+"xCalibration";
	public String folderPathAutomaticCalibration_testDir="input"+System.getProperty("file.separator")+"pafm"+System.getProperty("file.separator")+"auto_calib_test";
	
	public String folderPathXNewLogsCheck1="input"+System.getProperty("file.separator")+"pafm"+System.getProperty("file.separator")+"testing-exp1"; 
	public String folderPathXNewLogsCheck2="input"+System.getProperty("file.separator")+"pafm"+System.getProperty("file.separator")+"testing-exp2-conOld"; 
	public String folderPathXNewLogsCheck3="input"+System.getProperty("file.separator")+"pafm"+System.getProperty("file.separator")+"testing-exp2-conOld-b"; 
	
	public String eventName_fordensitycomputation="click_extdd-260";
	
	public String fsm2dotFileName_prefix="pafm10";
*/
	
/*
	//x AJAXIM
    public String folderPath="input"+System.getProperty("file.separator")+"ajaxim"+System.getProperty("file.separator")+"calibration2";
	
	public String folderPathAutomaticCalibration="input"+System.getProperty("file.separator")+"ajaxim"+System.getProperty("file.separator")+"auto_calib";
	public String folderPathAutomaticCalibration_subDir="input"+System.getProperty("file.separator")+"ajaxim"+System.getProperty("file.separator")+"auto_calib"+System.getProperty("file.separator")+"xCalibration";
	public String folderPathAutomaticCalibration_testDir="input"+System.getProperty("file.separator")+"ajaxim"+System.getProperty("file.separator")+"auto_calib_test";
	
	public String folderPathXNewLogsCheck1="input"+System.getProperty("file.separator")+"ajaxim"+System.getProperty("file.separator")+"testing-exp1"; 
	public String folderPathXNewLogsCheck2="input"+System.getProperty("file.separator")+"ajaxim"+System.getProperty("file.separator")+"testing-exp2-conOld"; 
	public String folderPathXNewLogsCheck3="input"+System.getProperty("file.separator")+"ajaxim"+System.getProperty("file.separator")+"testing-exp2-conOld-b"; 
	
	public String eventName_fordensitycomputation="click_extdd-260";
	
	public String fsm2dotFileName_prefix="ajaxim10";
*/
	
	
//--------		
    public int total_numberofcreatedlog=0;	
    public int[] t1_numberOfExpectedLogs=null;
    public int[] t2_numberOfExpectedLogs=null;
    public int[] ttest_numberOfExpectedLogs=null;
//--------	
	
	public String outputDirName="output_x"+X;
	
	public String generateTraceDirName="traceSim";
	public String fsm2dotFileName_final="";
	public String generateTraceFileName=fsm2dotFileName_final;
	
	public String getFolderPath() {
		return folderPath;
	}
	public String getFsm2dotFileName_prefix() {
		return fsm2dotFileName_prefix;
	}
	public int getMaxFilePemutations() {
		return maxFilePemutations;
	}
	public String getOutputDirName() {
		return outputDirName;
	}
	public int getX() {
		return X;
	}
	public int getMaxloop() {
		return maxloop;
	}

	public double getAlpha() {
		return alpha;
	}

	public String getFolderPathXNewLogsCheck(int index) {
		if (index==1) return folderPathXNewLogsCheck1;
		else if (index==2) return folderPathXNewLogsCheck2;
		else return folderPathXNewLogsCheck3;
	}

	public void setGenerateTraceFileName(String generateTraceFileName) {
		this.generateTraceFileName = generateTraceFileName;
	}

	/*public void setChange(boolean change) {
		this.change = change;
	}

	public void setEventName_fordensitycomputation(
			String eventName_fordensitycomputation) {
		this.eventName_fordensitycomputation = eventName_fordensitycomputation;
	}*/

	public void setFolderPath(String folderPath) {
		this.folderPath = folderPath;
	}
	
	
}
