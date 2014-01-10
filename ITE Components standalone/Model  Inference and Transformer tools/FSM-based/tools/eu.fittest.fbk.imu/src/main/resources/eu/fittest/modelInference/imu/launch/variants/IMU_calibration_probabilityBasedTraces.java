package eu.fittest.modelInference.imu.launch.variants;

import java.util.*;





import eu.fittest.modelInference.imu.test.ConvertPathToTeseCase;
import eu.fittest.modelInference.imu.test.TestCase;
import eu.fittest.modelInference.imu.modelAlgs.Path;
import eu.fittest.modelInference.imu.modelAlgs.Probabilistic_PathExtraction;
import eu.fittest.modelInference.imu.config.Config_Launcher;
import eu.fittest.modelInference.imu.test.WriteTrace;

import eu.fittest.modelInference.fsmInference.fsm.Fsm;

/**
 * 
 * 
 * @author Alessandro Marchetto
 *
 */
public class IMU_calibration_probabilityBasedTraces {
	Probabilistic_PathExtraction prob_pathExt=new Probabilistic_PathExtraction();
	Hashtable<Long,Double> event_probability;
	int maxLength_SinglePath;
	WriteTrace wIt=new WriteTrace();
	Config_Launcher cl=Config_Launcher.getInstace();
	
	Vector<TestCase> suite=null;
	Vector<Path> paths=null;
	
	public int[] generateTraces_withProbabilities(Hashtable<Long,Double> event_probability,Fsm fsm, int maxLength_SinglePath, long idEventToBeChanged, boolean writeOnFiles){
	
		this.event_probability=event_probability;
		this.maxLength_SinglePath=maxLength_SinglePath;
		int[] numberOfTraces=new int[3];
			
		int tmp1=0;
		int tmp2=0;
		int tmp3=0;
		int ev=0;
		
		suite=null;
		paths=null;
				
		
		if (writeOnFiles==false) {
			
			paths=extract_TCSinPath_DFS_withProbability(fsm,idEventToBeChanged);
			if (paths!=null) {
				
					tmp1=(int)(paths.size()*cl.t1_PercOfExpectedLogs);
					tmp2=(int)(paths.size()*cl.t2_PercOfExpectedLogs);
					tmp3=(paths.size()-(tmp1+tmp2));
					
					for (int i = 0; i < tmp1; i++) {
						ev=ev+paths.get(i).size();

					}
					numberOfTraces[0]=ev;
					
					ev=0;
					for (int i = tmp1; i < (tmp1+tmp2); i++) {
						ev=ev+paths.get(i).size();
					}
					numberOfTraces[1]=ev;
			
					ev=0;
					for (int i = (tmp1+tmp2); i < paths.size(); i++) {
						ev=ev+paths.get(i).size();
					}
					numberOfTraces[2]=ev;
		}
		else {
			
			suite=extract_TCS_DFS_withProbability(fsm,idEventToBeChanged);
			
			Vector<TestCase> tmpSuite;
			if (suite!=null) {
					tmp1=(int)(suite.size()*cl.t1_PercOfExpectedLogs);
					tmp2=(int)(suite.size()*cl.t2_PercOfExpectedLogs);
					tmp3=(suite.size()-(tmp1+tmp2));
	
					tmpSuite=new Vector<TestCase>();
					for (int i = 0; i < tmp1; i++) {
						tmpSuite.add(suite.get(i));
						ev=ev+suite.get(i).size();
					}
					numberOfTraces[0]=ev;
					wIt.writeInFiles(tmpSuite,cl.folderPathAutomaticCalibration,"log_ac");
			
					tmpSuite=new Vector<TestCase>();
			
					ev=0;
					for (int i = tmp1; i < (tmp1+tmp2); i++) {
						tmpSuite.add(suite.get(i));
						ev=ev+suite.get(i).size();
					}
					numberOfTraces[1]=ev;
					 wIt.writeInFiles(tmpSuite,cl.folderPathAutomaticCalibration_testDir,"log_ac");
			
					tmpSuite=new Vector<TestCase>();
			
					ev=0;
					for (int i = (tmp1+tmp2); i < suite.size(); i++) {
						tmpSuite.add(suite.get(i));
						ev=ev+suite.get(i).size();
				
					}
					numberOfTraces[2]=ev;
					wIt.writeInFiles(tmpSuite,cl.folderPathAutomaticCalibration_subDir,"log_ac");
			
				
				}
			}	
			
			cl.total_numberofcreatedlog=tmp1+tmp2+tmp3;
			cl.t1_numberOfExpectedLogs=new int[2];
			cl.t1_numberOfExpectedLogs[0]=0;
			cl.t1_numberOfExpectedLogs[1]=tmp1;
			cl.t2_numberOfExpectedLogs=new int[2];
			cl.t2_numberOfExpectedLogs[0]=tmp1;
			cl.t2_numberOfExpectedLogs[1]=tmp1+tmp2;
			cl.ttest_numberOfExpectedLogs=new int[2];
			cl.ttest_numberOfExpectedLogs[0]=tmp2;
			cl.ttest_numberOfExpectedLogs[1]=tmp2+tmp3;
			
			return numberOfTraces;
		}
		return null;
	
	}
	
	public Vector<TestCase> extract_TCS_DFS_withProbability(Fsm fsm, long idEventToBeChanged){
		 Vector<TestCase> tcs=new Vector<TestCase>();
		 TestCase tctmp;
		 
		 prob_pathExt=new Probabilistic_PathExtraction(); 
		 
		 Vector<Path> paths=prob_pathExt.pathsExtraction_withProbability(fsm,this.event_probability,maxLength_SinglePath,idEventToBeChanged);
		
		 for (int i = 0; i < paths.size(); i++) {
			 tctmp=ConvertPathToTeseCase.convertP2TC(paths.get(i), fsm.transitions);
			 //if (contained(tcs,tctmp)==false) tcs.add(tctmp);
			 tcs.add(tctmp);
 
		}
		
		 return tcs;
	 }
	
	public Vector<Path> extract_TCSinPath_DFS_withProbability(Fsm fsm, long idEventToBeChanged){
	 
		 prob_pathExt=new Probabilistic_PathExtraction(); 
		 Vector<Path> paths=prob_pathExt.pathsExtraction_withProbability(fsm,this.event_probability,maxLength_SinglePath,idEventToBeChanged);
			
		 return paths;
	}
	
	public boolean contained(Vector<TestCase> tcs, TestCase tc){
		for (int i = 0; i < tcs.size(); i++) {
			if (tcs.get(i).isEqual(tc)) return true;
		}
		return false;
	}
	
	public Vector<Long> extractPath_DFS_withProbability(Hashtable<Long,Double> event_probability,Fsm fsm){
		this.event_probability=event_probability;
					
		Path p=prob_pathExt.pathExtraction_withProbability(fsm,this.event_probability,1,maxLength_SinglePath);
		
		 if (p!=null) return p.getPathTransitionsVector();
		 
		 return null;
	 }
	
	
}
