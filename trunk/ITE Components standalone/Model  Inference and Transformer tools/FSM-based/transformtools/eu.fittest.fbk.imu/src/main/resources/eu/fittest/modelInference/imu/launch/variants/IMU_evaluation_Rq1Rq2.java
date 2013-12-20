package eu.fittest.modelInference.imu.launch.variants;

import java.util.*;


import eu.fittest.modelInference.imu.java_MKV.SteadyStateProbability;
import eu.fittest.modelInference.imu.modelAlgs.*;
import eu.fittest.modelInference.imu.utility.Utility;
import eu.fittest.modelInference.imu.config.Config_Launcher;
import eu.fittest.modelInference.imu.config.LoadConfiguration;
import eu.fittest.modelInference.imu.launch.*;
import eu.fittest.modelInference.imu.launch.imuProxy.IMUproxy_ForCalibration;
import eu.fittest.modelInference.imu.launch.imuProxy.IMUproxy_ForCheckNewLogs;

import Jama.Matrix;

import eu.fittest.modelInference.fsmInference.manager.*;
import eu.fittest.modelInference.fsmInference.fsm.*;

/**
 * Evaluation for Rq1 and Rq2
 * 
 * @author Alessandro Marchetto
 *
 */
public class IMU_evaluation_Rq1Rq2 {
	IMUproxy_ForCalibration imu;
	SteadyStateProbability ssp=new SteadyStateProbability();
	Config_Launcher cl=Config_Launcher.getInstace();
	IMU_calibration_probabilityBasedTraces tracesGen=new IMU_calibration_probabilityBasedTraces();
	Transitions transitions_initial;
	Utility utils=new Utility();
	Fsm fsm_initial;
	IMU_checkNewLogs checkNewLogs=new IMU_checkNewLogs();
	IMU_evaluation_Rq1Rq2 imuCalibration;
	
	LoadConfiguration predefinedConfig=new LoadConfiguration();
	
	public static void main(String[] args) {
		IMU_evaluation_Rq1Rq2 imuCalibration=new IMU_evaluation_Rq1Rq2();
		imuCalibration.repeat();
	}
	
	void repeat(){
		
		if (!predefinedConfig.loadConfiguration()){
			System.out.println("Configuration NOT loaded from XML ... the default one is used!");
			imuCalibration=new IMU_evaluation_Rq1Rq2();
			imuCalibration.run(-1);
		}else {
			System.out.println("\nIteration,Event,InitalP,NewP,(a)Fsm,(a)nEvents_T1T2,(a)nEvents_all,(a)Xret,(a)Xact,(a)nDel,(a)nAdd"); 
			
			int tmpNumberofEventsToCheck=cl.numberOfEventsToCheckRandomly;
			if (cl.eventsTocheck!=null){
				if (cl.eventsTocheck.size()>0){
					tmpNumberofEventsToCheck=cl.eventsTocheck.size();
				}
			}

			
			for(int indexOfEventToUse=0; indexOfEventToUse< tmpNumberofEventsToCheck; indexOfEventToUse++){
				//System.out.println("******* NEW EVENT (number="+indexOfEventToUse+")"); 
				//cl.evaluationA_distanceAtWhichSelectEvent=-1; //fixed per Arthur
				for (double i = 0.0; i < 1.0; i=i+0.1) {
					if (i==0.0) cl.evaluationA_percent4xfixed=-1.0;
					else cl.evaluationA_percent4xfixed=i;
					imuCalibration=new IMU_evaluation_Rq1Rq2();
					imuCalibration.run(indexOfEventToUse);
				
				}
				//System.out.println(" ******* NEW EVENT (number="+indexOfEventToUse+")");
			}
		}

	}
	
	public boolean run(int indexOfEventToUse){
		//System.out.println("\nIteration,Event,InitalP,NewP,(a)Fsm,(a)nEvents_T1T2,(a)nEvents_all,(a)Xret,(a)Xact,(a)nDel,(a)nAdd"); //newPrint
	
		Hashtable<Long,Double> event_probabilityMutated;
		
		Hashtable<Long,Double> event_probabilityUniform=calibration_UniformRun();
		
		
		long idEventToBeChanged=-1; //prendere come parametro
		if (cl.eventsTocheck==null){
			idEventToBeChanged=selectEventToBeChagned_QuasiRandom(event_probabilityUniform);
		}else if (cl.eventsTocheck.size()==0){
			idEventToBeChanged=selectEventToBeChagned_QuasiRandom(event_probabilityUniform);
		}else {
			if ((indexOfEventToUse<cl.eventsTocheck.size())&&(indexOfEventToUse>-1)){ 
				//System.out.print(" fo="+indexOfEventToUse);
				idEventToBeChanged=selectEventToBeChagned_ByConfiguration(event_probabilityUniform,indexOfEventToUse);
				//System.out.print(" outfo="+idEventToBeChanged);
				}
			else idEventToBeChanged=-1;
		}
		
		if (idEventToBeChanged==-1) idEventToBeChanged=selectEventToBeChagned_QuasiRandom(event_probabilityUniform);
		if (idEventToBeChanged==-1) return false;
		
		
		for (int i = 0; i < cl.probabilitiesCoefficientArray.length; i++) {
		
			//System.out.print(cl.evaluationA_distanceAtWhichSelectEvent+","+i); //newPrint
			System.out.print(i); //newPrint
			
			utils.emptyDirectory(cl.folderPathAutomaticCalibration);
			utils.emptyDirectory(cl.folderPathAutomaticCalibration_testDir);
			
			event_probabilityMutated=mutateProbability(event_probabilityUniform,idEventToBeChanged,cl.probabilitiesCoefficientArray[i]); 

			if (event_probabilityMutated!=null){
				if (event_probabilityMutated.size()>0){
					
					eventBased_analysis(event_probabilityMutated,idEventToBeChanged, true, false);
					
				}
			}
			
			
		}
		return true;
	}
	
	void eventBased_analysis(Hashtable<Long,Double> event_current_probability, long idEventToBeChanged, boolean print, boolean tracesOnfiles){
		int[] numberofEvents;
		int traces=0;
		int xreturned=0;
		int xactuallyused=0;
		int rem=0; int add=0; int fsmsize=0;
		int[] ops=new int[3];
		int[] retrerun=new int[3];
		int counter=0;
		int sumNumberofEvents=0;
		int sumNumberofEventsT1T2=0;
		//int sumNumberofEventsTtest=0;
				
	
		tracesGen=new IMU_calibration_probabilityBasedTraces();
		for (int j = 0; j < cl.evaluationA_iterations_perEvent; j++) {
			
			utils.emptyDirectory(cl.folderPathAutomaticCalibration);
			utils.emptyDirectory(cl.folderPathAutomaticCalibration_testDir);
			
		
			numberofEvents=tracesGen.generateTraces_withProbabilities(event_current_probability, fsm_initial, cl.evaluationA_maxLengthOfGeneratedTests, idEventToBeChanged,tracesOnfiles);
			//System.out.print("[["+(numberofEvents[0]+numberofEvents[1]+numberofEvents[2])+"]]");
			if (numberofEvents!=null){
				loadNewTraces_XCalibration(tracesOnfiles);
				
				if (!print) retrerun=rerunCalibartion(true,numberofEvents,tracesOnfiles,tracesGen.paths);
				else retrerun=rerunCalibartion(false,numberofEvents,tracesOnfiles,tracesGen.paths);
							
				//System.out.print("[["+(numberofEvents[0]+numberofEvents[1]+numberofEvents[2])+"]]");
			    if (retrerun!=null){
				ops=loadNewTraces_checkNewLogs_XTesting(tracesOnfiles,tracesGen.paths);
				
				sumNumberofEvents=sumNumberofEvents+numberofEvents[0]+numberofEvents[1]+numberofEvents[2];
				sumNumberofEventsT1T2=sumNumberofEventsT1T2+numberofEvents[0]+numberofEvents[1];
				xreturned=xreturned+retrerun[0]; 
				fsmsize=fsmsize+retrerun[1];
				rem=rem+ops[2];
				add=add+ops[1];
				xactuallyused=xactuallyused+ops[0];
				counter++;
				
			}
			//j++;
			} 
		}
		
		if (print){
		if (counter>0){
			 //media
			double counterDouble=new Double(counter).doubleValue();
			System.out.print(","+(new Double(fsmsize)/counterDouble)+","+
				(new Double(sumNumberofEventsT1T2)/counterDouble)+","+
				(new Double(sumNumberofEvents)/counterDouble)+","+
				(new Double(xreturned)/counterDouble)+","+
				(new Double(xactuallyused)/counterDouble)+","+
				(new Double(rem)/counterDouble)+","+
				(new Double(add)/counterDouble)+
				"\n");		
		}
		else {
			System.out.print(", , , , , ,\n");
			}
		}
	}
	
	
	public Hashtable<Long,Double> calibration_UniformRun(){
		
		utils.emptyDirectory(cl.folderPathAutomaticCalibration);
		utils.emptyDirectory(cl.folderPathAutomaticCalibration_testDir);
		
		//store iniziale
		FSMmanager m=createFSM();
		fsm_initial=m.fsmAllInOne;
		double[][] transitionMatrix=initialSetting_onRealTraces(m);
		//fsm_initial=imu.fsmmanager.fsmAllInOne;
		transitions_initial=new Transitions();
		for (int i = 0; i < fsm_initial.transitions.size(); i++) {
			//transitions_initial.addTransition(fsm_initial.transitions.getTransitions().get(i).clone()); //21 june
			//transitions_initial.addTransition(fsm_initial.transitions.getTransitions().get(i));
			transitions_initial.addTransition(fsm_initial.transitions.getTransitions().get(i));
		}
		
		Hashtable<Long,Double> event_probability=GetAndConvertFormatOfEventProbability(m);//<evento,probabilit�>
		
		//int[] numTracesGenerated=tracesGen.generateTraces_withProbabilities(event_probability, fsm_initial, cl.evaluationA_numberOfTracesForCalibation, cl.evaluationA_maxLengthOfGeneratedTests);
		//loadNewTraces_XCalibration();
		//rerunCalibartion();
		
		eventBased_analysis(event_probability,-1, false, false);
		
		return event_probability;
				
	}
	
	public long getIElementInHash(Hashtable<Long,Double> h,int indexToGet){
		 Set<Long> set = h.keySet();
		 long key_eId;
		 Iterator<Long> itr = set.iterator();
		 int i=0;
		 while (itr.hasNext()) {
			 key_eId= (itr.next()).longValue();
			 if (i==indexToGet) {
				 //value_eProb=(h.get(key_eId)).doubleValue();
				 return key_eId;
			 }
			 i++;
		 }
		 return -1;
	}
	
	
	public long selectEventToBeChagned_Random(Hashtable<Long,Double> tobeMutated){
		return getIElementInHash(tobeMutated,utils.randomInt(tobeMutated.size(), false));
	}
	public long selectEventToBeChagned_QuasiRandom(Hashtable<Long,Double> tobeMutated){
		/*
		Vector<Long> idTransitionOfAPath=tracesGen.extractPath_DFS_withProbability(tobeMutated,fsm_initial);
		if (idTransitionOfAPath.size()>=cl.evaluationA_distanceAtWhichSelectEvent) {
			return idTransitionOfAPath.get(cl.evaluationA_distanceAtWhichSelectEvent);
		}
		else return idTransitionOfAPath.get(idTransitionOfAPath.size()-1);
		*/
		Vector<Long> idTransitionOfAPath=tracesGen.extractPath_DFS_withProbability(tobeMutated,fsm_initial);
		int dist=utils.randomInt(idTransitionOfAPath.size(), true);
		return idTransitionOfAPath.get(dist);
	}
	
	public long selectEventToBeChagned_ByConfiguration(Hashtable<Long,Double> tobeMutated,int indexOfEventToUse){
		//fsm_initial
		
		String[] eventToconsider=cl.eventsTocheck.get(indexOfEventToUse); //indexOfEventToUse
		
		//assemblo gli elementi in fomato per ricerca in mem
		String[] stBefore=utils.removeEmptyItems(eventToconsider[0].split("\\s*;__;\\s*"));
		String[] eventTo=utils.removeEmptyItems(eventToconsider[1].split("\\s*;__;\\s*"));
		String[] stAfter=utils.removeEmptyItems(eventToconsider[2].split("\\s*;__;\\s*"));
		
		//System.out.println(" <"+stBefore.length+" "+eventTo.length+" "+stAfter.length+"> ");
		//System.out.println(" <"+stBefore[0]+" "+eventTo[0]+" "+stAfter[0]+"> ");
		
		String[] stBefore2;
		String[] eventTo2;
		String[] stAfter2;
		
		 Set<Long> set = tobeMutated.keySet();
		 long idCurrentTransition;
		 Iterator<Long> itr = set.iterator();
		 Transition t;
		 while (itr.hasNext()) {
			 
			 idCurrentTransition= (itr.next()).longValue();
			 t=fsm_initial.transitions.getTransitionById(idCurrentTransition);
			 try {
				 
				 stBefore2=utils.removeEmptyItems(fsm_initial.states.getStateById(t.getIdStateSource()).getStateContent());
				 eventTo2=utils.removeEmptyItems(t.getTransitionContent());
				 stAfter2=utils.removeEmptyItems(fsm_initial.states.getStateById(t.getIdStateTarget()).getStateContent());
				 
				 //System.out.println(" <"+eventTo[0]+"-"+eventTo2[0]+"> ");
				 if (utils.equalContent(eventTo,eventTo2)) {
					 
					 if (utils.equalContent(stBefore, stBefore2)) {
						 
							if (utils.equalContent(stAfter, stAfter2)){
						 		return idCurrentTransition;
						 		}
						 	}
				}
				 
			 }catch(Exception e){
				 //....
			 }
			 
		 }
		
		return -1;
	}

	public Hashtable<Long,Double> mutateProbability(Hashtable<Long,Double> transitionMatrix_hash,long key_EventToBeMutated_Id,double probabilityCoefficient){
		Hashtable<Long,Double> mutated=new Hashtable<Long,Double>();
		 Set<Long> set = transitionMatrix_hash.keySet();
		 long key;double value;
		 Iterator<Long> itr = set.iterator();
		 while (itr.hasNext()) {
			 key= (itr.next()).longValue();
			 value=(transitionMatrix_hash.get(key)).doubleValue();
			 mutated.put(new Long(key), new Double(value));
		 }
		
		if (key_EventToBeMutated_Id==-1) return null;
			
		long ids=-1;
		ids=transitions_initial.getTransitionById(key_EventToBeMutated_Id).getIdStateSource();
		
		if (ids==-1) return null;
		
		Vector<Transition> outs=new Vector<Transition>();
		
		outs=transitions_initial.getTransitionsBy_SourceId(ids);
				
		if ((outs.size()==0)||(outs.size()==1)) return null;
				
		String mutatedEventName=(transitions_initial.getTransitionById(key_EventToBeMutated_Id)).getTransitionContent()[0];
		double oldProb=(mutated.get(key_EventToBeMutated_Id).doubleValue());
		double newProb=(mutated.get(key_EventToBeMutated_Id).doubleValue())/(probabilityCoefficient);
		
	//System.out.println(" [ON="+oldProb+" NP="+newProb+"] ");
		
		double yremained=oldProb-newProb;
		double uniRemined=yremained/(outs.size()-1);
		double d=0;
				
		for (Transition transitionOut : outs) {
			if (transitionOut.getidTransition()!=key_EventToBeMutated_Id){
				d=(mutated.get(transitionOut.getidTransition())).doubleValue()+uniRemined;
				mutated.put(transitionOut.getidTransition(),d);
			}else {
				mutated.put(transitionOut.getidTransition(),new Double(newProb));
			}
		}
		
		
		//System.out.println("..-> mutated event:"+mutatedEventName+"("+key_EventToBeMutated_Id+") org prob="+pr[tobeChanged]+" new prob="+newProb);//oldPrint
		//System.out.print(","+mutatedEventName+"("+key_EventToBeMutated_Id+"),"+oldProb+","+newProb);
		System.out.print(","+mutatedEventName+","+oldProb+","+newProb);
		
		/* //copy back 
		 mutated=new Hashtable<Long,Double>();
		 Set<Long> set = transitionMatrix_hash.keySet();
		 long key;double value;
		 Iterator<Long> itr = set.iterator();
		 while (itr.hasNext()) {
			 key= (itr.next()).longValue();
			 value=(transitionMatrix_hash.get(key)).doubleValue();
			 mutated.put(new Long(key), new Double(value));
		 }*/

		if (mutated.size()>0) return mutated;
		return null;
	}
	
	int x_tobeReturned;
	double alpha;
	int X;
	int maxloop;
	int maxFilePemutations;
	String 	folderPath; 
	String fsm2dotFileName_prefix;
	String outputDirName;
	double[][] transitionMatrix;
	int numTransT;
	double[][] transitionMatrix_variant;
	Matrix mat;
	
	public FSMmanager createFSM(){
		IMUproxy_ForCalibration imu1=new IMUproxy_ForCalibration();
		
		x_tobeReturned=-1;
		
		alpha=cl.getAlpha();
		X=cl.getX();
		maxloop=cl.getMaxloop();
		maxFilePemutations=cl.getMaxFilePemutations();
		folderPath=cl.getFolderPath(); 
		fsm2dotFileName_prefix=cl.getFsm2dotFileName_prefix();
		outputDirName=cl.getOutputDirName();
		
		imu1.setUpParams(X,maxFilePemutations,folderPath,fsm2dotFileName_prefix,outputDirName,true,maxloop,true);
	
		imu1.incrementalModelInferenceOnASetOfLogs_usedForCalibration(true, 2, 0, true, null, null);
		//System.out.println("initial - number of events in the logs ->"+imu1.fsmmanager.fsmAllInOne.eventsinlogs);
	
		return imu1.fsmmanager;
	}
	
	
	public double[][] initialSetting_onRealTraces(FSMmanager m){
				
		transitionMatrix=fillUniformTransitionMatrix(m);
		numTransT=transitionMatrix.length;

		mat=null;
		
		return transitionMatrix;
	}
	
	public void loadNewTraces_XCalibration(boolean tracesOnfiles){
		imu=new IMUproxy_ForCalibration();
		imu.cleanFSM();
		x_tobeReturned=-1;
		
		imu.logseqs.forceNewInstace();
		imu.setUpParams(X,maxFilePemutations,cl.folderPathAutomaticCalibration,fsm2dotFileName_prefix,outputDirName,true,maxloop,tracesOnfiles);
	}
	
	public int[] loadNewTraces_checkNewLogs_XTesting(boolean tracesOnfiles,Vector<Path> suite){
    	IMUproxy_ForCheckNewLogs imu_test=new IMUproxy_ForCheckNewLogs(); 
		imu_test.cleanFSM();
		imu_test.logseqs.forceNewInstace();
		
		int actuallyUsedX=x_tobeReturned;
		if (cl.evaluationA_percent4xfixed==-1.0)
			imu_test.setUpParams(x_tobeReturned,1,cl.folderPathAutomaticCalibration_testDir,fsm2dotFileName_prefix,outputDirName,true,maxloop,tracesOnfiles); //DA USARE
		else {
			//per usare x fisso
			actuallyUsedX=(int)(x_tobeReturned*cl.evaluationA_percent4xfixed);
			imu_test.setUpParams(actuallyUsedX,1,cl.folderPathAutomaticCalibration_testDir,fsm2dotFileName_prefix,outputDirName,true,maxloop,tracesOnfiles); 
		}
		imu_test.incrementalModelInferenceOnASetOfLogs_usedForCheckNewLogs(0, tracesOnfiles, suite, fsm_initial);
		int[] ops=imu_test.getNumOfOps();
		//System.out.println(" Model Update===>actuallyUsedX="+actuallyUsedX+" Num Add ops="+ops[0]+" ; Num Rem ops="+ops[1]);
		
		int[] ops2=new int[4];
		ops2[0]=actuallyUsedX;
		ops2[1]=ops[0];
		ops2[2]=ops[1];
		return ops2;
	}
	
	
	public int[] rerunCalibartion(boolean uniformTransitionMatrix, int[] numberofEvents, boolean readLogsFromFiles, Vector<Path> suite){
		
		imu.incrementalModelInferenceOnASetOfLogs_usedForCalibration(true, 2, 0,readLogsFromFiles, suite, fsm_initial);
		//int numberEventsInLogsT1T2=imu.fsmmanager.countEventsInLogs;
		
		//CODICE CORRETTO
		//System.out.print("["+(numberofEvents[0]+numberofEvents[1])+","+imu.fsmmanager.fsmAllInOne.eventsinlogs+"]");
		//System.out.print("['(number of events in the logs ->"+imu.fsmmanager.fsmAllInOne.addops+"']");
		
		if (uniformTransitionMatrix) transitionMatrix=fillUniformTransitionMatrix(imu.fsmmanager);
		else transitionMatrix=fillFrequencyBasedTransitionMatrix(imu.fsmmanager);
		numTransT=transitionMatrix.length;
		
		if (transitionMatrix==null) return null;
		if (transitionMatrix.length==0) return null;
			
		
		mat=ssp.computeSteadyStateMatrix_byIterations(transitionMatrix.length,transitionMatrix);

		double[][] matarray=mat.getArray();
	
		double Tm=computeTm(matarray,alpha,transitionMatrix);
		//System.out.print(" tm="+Tm+") ");
	
		if ((numberofEvents[0]<Tm)||(numberofEvents[1]<Tm)) {
		
				//System.out.println("Insuff. number of traces, required="+Tm+"insteadof="+numberEventsInLogsT1+" "+numberEventsInLogsT2);
				return null;
		}else {
			
			//System.out.print("1");
			imu.incrementalModelInferenceOnASetOfLogs_usedForCalibration(true, 0, 0, readLogsFromFiles, suite, fsm_initial);
			//int numberEventsInLogsT1=imu.fsmmanager.fsmAllInOne.numberoflinesinlog;
			
			int[] returnedFromFSMGeneration=imu.incrementalModelInferenceOnASetOfLogs_usedForCalibration(false, 3, 0, readLogsFromFiles,suite, fsm_initial);
			//System.out.print("3");
			if (returnedFromFSMGeneration==null) return null; 
				
			x_tobeReturned=returnedFromFSMGeneration[0];
			int fsmSize=returnedFromFSMGeneration[1];
			//System.out.print("["+numberEventsInLogsT1T2+"]");
			//int numberEventsInLogsT2=imu.fsmmanager.fsmAllInOne.numberoflinesinlog;			
			
			int[] ret2=new int[2];
			ret2[0]=x_tobeReturned;
			ret2[1]=fsmSize;
			return ret2;
		}
		
		
		
		
	}
	
	
	private double computeTm(double[][] matarray,double alpha, double[][] transitionMatrix){
		double Tm=0;
		double max=0;
		double tmp=0;
				
		for (int i = 0; i < matarray.length; i++) {
			//System.out.print(" %"+matarray[i][0]+"% ");
			for (int j = 0; j < transitionMatrix[i].length; j++) {
				if ((matarray[i][0]!=0)&&(transitionMatrix[i][j]!=0)){
					tmp=(new Double(1).doubleValue())/(matarray[i][0]*transitionMatrix[i][j]);
					if (max<tmp) max=tmp;
					//if (min>transitionMatrix[i][j])  min=transitionMatrix[i][j];
				}
					
			}
			
		}
		//System.out.print(" ("+min+") ");
		
		Tm=alpha*max;
		return Tm;
	}
	
	private Hashtable<Long,Double> GetAndConvertFormatOfEventProbability(FSMmanager fsmmanager){
		
		double[] event_probs=getProbabilityForEvent(fsmmanager);
		
		Hashtable<Long,Double> event_probability=new Hashtable<Long,Double>();
		
		for (int e = 0; e < fsmmanager.fsmAllInOne.transitions.getTransitions().size(); e++) {
			event_probability.put(new Long(fsmmanager.fsmAllInOne.transitions.getTransitions().get(e).getidTransition()), 
					new Double(event_probs[e]));
		}
	
		return event_probability;
	}
	
	private double[] getProbabilityForEvent(FSMmanager fsmmanager){
		double[] p=new double[fsmmanager.fsmAllInOne.transitions.getTransitions().size()];
		Vector<Transition> outgoing_sourceSt_E;
		
		for (int e = 0; e < fsmmanager.fsmAllInOne.transitions.getTransitions().size(); e++) {
			outgoing_sourceSt_E=fsmmanager.fsmAllInOne.transitions.getTransitionsBy_SourceId(fsmmanager.fsmAllInOne.transitions.getTransitions().get(e).getIdStateSource());
			p[e]=1/(new Double(outgoing_sourceSt_E.size()).doubleValue());
		}
		return p;
	}

	//fillTransitionMatrix
	private double[][] fillUniformTransitionMatrix(FSMmanager fsmmanager){
		
		double[][] transitionMatrix=new double[fsmmanager.fsmAllInOne.states.size()][fsmmanager.fsmAllInOne.states.size()];
		
		//fsmmanager.fsmAllInOne.states.getStates()
		State t;
		
		for (int s_r = 0; s_r < fsmmanager.fsmAllInOne.states.size(); s_r++) {
			State source=fsmmanager.fsmAllInOne.states.getStates().get(s_r);
			Vector<Transition> outsTrans=fsmmanager.fsmAllInOne.transitions.getTransitionsBy_SourceId(source.getId());
			Vector<State> tstate=new Vector<State>();
			for (Transition transition : outsTrans) {
				tstate.add(fsmmanager.fsmAllInOne.states.getStateById(transition.getIdStateTarget()));
			}
			
			for (int s_c = 0; s_c < fsmmanager.fsmAllInOne.states.size(); s_c++) {
				transitionMatrix[s_r][s_c]=0;
				t=fsmmanager.fsmAllInOne.states.getStates().get(s_c);
				if (contains(tstate,t)){
					transitionMatrix[s_r][s_c]=(new Double(1).doubleValue())/ (new Double(outsTrans.size()).doubleValue());;
				}
				
			}
		}
			
		return transitionMatrix;
	}
	
	
  private double[][] fillFrequencyBasedTransitionMatrix(FSMmanager fsmmanager){
		
		double[][] transitionMatrix=new double[fsmmanager.fsmAllInOne.states.size()][fsmmanager.fsmAllInOne.states.size()];
		
		//fsmmanager.fsmAllInOne.states.getStates()
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
