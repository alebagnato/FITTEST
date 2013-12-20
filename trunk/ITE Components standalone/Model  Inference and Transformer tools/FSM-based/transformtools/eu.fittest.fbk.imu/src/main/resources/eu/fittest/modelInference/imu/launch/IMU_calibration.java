package eu.fittest.modelInference.imu.launch;

import java.util.Vector;

import eu.fittest.modelInference.imu.config.Config_Launcher;
import eu.fittest.modelInference.imu.config.LoadConfiguration;
import eu.fittest.modelInference.imu.launch.imuProxy.IMUproxy_ForCalibration;

import eu.fittest.modelInference.imu.java_MKV.SteadyStateProbability;
import Jama.Matrix;

import eu.fittest.modelInference.fsmInference.manager.FSMmanager;
import eu.fittest.modelInference.fsmInference.fsm.State;
import eu.fittest.modelInference.fsmInference.fsm.Transition;

/**
 * Calibration (alg.3?). 
 * It works with files and it turns back 'X' (expiration time).
 * 
 * @author Alessandro Marchetto
 *
 */
public class IMU_calibration {
	IMUproxy_ForCalibration imu;
	SteadyStateProbability ssp=new SteadyStateProbability();
	Config_Launcher cl=Config_Launcher.getInstace();
	LoadConfiguration predefinedConfig=new LoadConfiguration();
		
	public static void main(String[] args) {
		IMU_calibration imuCalibration=new IMU_calibration();
		imuCalibration.load();
		System.out.println(" computed X = "+imuCalibration.calibration());
	}
	
	public void load(){
		if (!predefinedConfig.loadConfiguration()){
			System.out.println("Configuration NOT loaded from XML ... the default one is used!");
		}
	}
	
	public int calibration(){
		imu=new IMUproxy_ForCalibration();
			
		int x_tobeReturned=-1;
		
		double alpha=cl.getAlpha();
		int X=cl.getX();
		int maxloop=cl.getMaxloop();
		int maxFilePemutations=cl.getMaxFilePemutations();
		String 	folderPath=cl.getFolderPath(); 
		String fsm2dotFileName_prefix=cl.getFsm2dotFileName_prefix();
		String outputDirName=cl.getOutputDirName();
		
		imu.setUpParams(X,maxFilePemutations,folderPath,fsm2dotFileName_prefix,outputDirName,true,maxloop,true);
	
		imu.incrementalModelInferenceOnASetOfLogs_usedForCalibration(true, 2, 0, true, null, null);
		int numEventsInLog=imu.fsmmanager.fsmAllInOne.eventsinlogs;
		
		double[][] transitionMatrix=fillFrequencyBasedTransitionMatrix(imu.fsmmanager);
			
		Matrix mat=ssp.computeSteadyStateMatrix_byIterations(transitionMatrix.length,transitionMatrix);
		double[][] matarray=mat.getArray();
		
		double Tm=computeTm(matarray,alpha,transitionMatrix); //
		//System.out.println("Tm="+Tm);
			
		if ((numEventsInLog*cl.t1_PercOfExpectedLogs<Tm)||
				(numEventsInLog*cl.t2_PercOfExpectedLogs<Tm)) {
			
			return -1;
			}	
		else {
			
			imu.incrementalModelInferenceOnASetOfLogs_usedForCalibration(true, 0, 0, true, null, null);
			int ret[]=imu.incrementalModelInferenceOnASetOfLogs_usedForCalibration(false, 3, 0, true, null, null);
			
			x_tobeReturned=ret[0];
			return x_tobeReturned;
			}

	}

	
	
	private double computeTm(double[][] matarray,double alpha, double[][] transitionMatrix){
		double Tm=0;
		double max=0;
		double tmp=0;
		
		for (int i = 0; i < matarray.length; i++) {
			for (int j = 0; j < transitionMatrix[i].length; j++) {
				if ((matarray[i][0]!=0)&&(transitionMatrix[i][j]!=0)){
					tmp=(new Double(1).doubleValue())/(matarray[i][0]*transitionMatrix[i][j]);
					if (max<tmp) max=tmp;
				}
					
			}
			
		}
		
		Tm=alpha*max;
		return Tm;
	}
	
	 private double[][] fillFrequencyBasedTransitionMatrix(FSMmanager fsmmanager){
			
			double[][] transitionMatrix=new double[fsmmanager.fsmAllInOne.states.size()][fsmmanager.fsmAllInOne.states.size()];
			
			State target;
			Vector<Transition> vt;
			
			for (int s_r = 0; s_r < fsmmanager.fsmAllInOne.states.size(); s_r++) {
				State source=fsmmanager.fsmAllInOne.states.getStates().get(s_r);
				Vector<Transition> outsTrans=fsmmanager.fsmAllInOne.transitions.getTransitionsBy_SourceId(source.getId());
				Vector<State> tstate=new Vector<State>();
				for (Transition transition : outsTrans) {
					tstate.add(fsmmanager.fsmAllInOne.states.getStateById(transition.getIdStateTarget()));
				}
				double freqSum_fromSource=0;
				for (State stateTarget : tstate) {
					vt=fsmmanager.fsmAllInOne.transitions.getTransitionsBySourceId_and_TargetId(source.getId(), stateTarget.getId());
					for (Transition t : vt) {
						freqSum_fromSource=freqSum_fromSource+t.getExcFreqTransition();
					}
				}
				
				double freqSumXcuttentEvent=0;
				for (int s_c = 0; s_c < fsmmanager.fsmAllInOne.states.size(); s_c++) {
					transitionMatrix[s_r][s_c]=0;
					target=fsmmanager.fsmAllInOne.states.getStates().get(s_c);
					if (contains(tstate,target)){
						vt=fsmmanager.fsmAllInOne.transitions.getTransitionsBySourceId_and_TargetId(source.getId(), target.getId());
						for (Transition t : vt) {
							freqSumXcuttentEvent=freqSumXcuttentEvent+t.getExcFreqTransition();
						}
						transitionMatrix[s_r][s_c]= freqSumXcuttentEvent/ freqSum_fromSource;
					}
					
				}
			}
							
			return transitionMatrix;
		}
	 
	 boolean contains(Vector<State> tstate,State s){
			for (State state : tstate) {
				if (state.equals(s)) return true;
			}
			return false;
		}
	
}
