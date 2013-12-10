package eu.fittest.eventSequenceGenerator.eventSeqGenerator;


import java.io.File;


import java.util.*;

import eu.fittest.eventSequenceGenerator.randomCoverage.PathsGenerator;
import eu.fittest.eventSequenceGenerator.semanticInteraction.*;
import eu.fittest.eventSequenceGenerator.visitor.*;
//import eu.fittest.eventSequenceGenerator.data.FSM;
import eu.fittest.eventSequenceGenerator.data.Edge;
//import eu.fittest.eventSequenceGenerator.data.FSM;
import eu.fittest.eventSequenceGenerator.data.FSM;
import eu.fittest.eventSequenceGenerator.data.Node;
//import eu.fittest.eventSequenceGenerator.data.Path;
import eu.fittest.eventSequenceGenerator.diversity.*;

import eu.fittest.modelInference.imu.utility.*;
import eu.fittest.modelInference.imu.launch.variants.IMU_calibration_probabilityBasedTraces;

import eu.fittest.modelInference.fsmInference.fsm.*;
import eu.fittest.modelInference.fsmInference.manager.FSMmanager;

/**
 * 
 * 
 * @author Alessandro Marchetto
 *
 */
public class EventSeqGenerator {
	Transitions transitions_initial;
	Utility utils=new Utility();
	Fsm fsm_initial;
	//Vector<Path> paths; //sequences
	Vector<eu.fittest.eventSequenceGenerator.data.Path> paths;  //sequences
	
	double[][] transitionMatrix;
	
	
	//coverage
	IMU_calibration_probabilityBasedTraces tracesGen=new IMU_calibration_probabilityBasedTraces();
	PathsGenerator pathsGen=new PathsGenerator();
	
	//visitor
	BreadthFirstVisit bfv=new BreadthFirstVisit();
	BreadthFirstVisitWithLoop bfvwl=new BreadthFirstVisitWithLoop();
	BreadthFirstVisitWithGlobalLoop bfvwgl=new BreadthFirstVisitWithGlobalLoop();
	
	//sem
	SEQsextractor seq=new SEQsextractor();
	SEMsextractor sem=new SEMsextractor();
	SEMsextractor_onlyLastEvent sem_onlylastevent=new SEMsextractor_onlyLastEvent(); 
	ALTsextractor alt=new ALTsextractor();
	
	//diversity
	SA sa=new SA();
	SA_onlyLastEvent sa_onlyLastEvent=new SA_onlyLastEvent();
	
	int N_max=100;
	double t_initial=5;
	int SuiteIt_max=500; //(2*sizeOf(suite))
	int TcIt_max=10;
	String typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();
	
	//to be customized for each run
	private String folderTraces="input"+System.getProperty("file.separator")+"log";;
	private String folderOriginalFSM="input"+System.getProperty("file.separator")+"tudu_actual";
	private String folderNewEventSequences="generatedPaths";
	private String typeOfCoverage="coverage";
	private int maxLengthGeneratedTraces=15;
	private int k=2;
	private String typeOfVisit="BreadthFirstVisit";
	private String fileNameOfAbstractTC="abstractTC";
		  
	//setup
	public void setUp(String folderOriginalFSM, String folderTraces, String folderNewEventSequences, String fileNameOfAbstractTC, String typeOfCoverage,int k,String typeOfVisit, int N_max, double t_initial, int SuiteIt_max, int TcIt_max, String typeOfFitness){
		this.folderOriginalFSM=folderOriginalFSM;
		this.folderNewEventSequences=folderNewEventSequences;
		this.folderTraces=folderTraces;
		this.typeOfCoverage=typeOfCoverage;
		this.k=k;
		this.typeOfVisit=typeOfVisit;
		this.fileNameOfAbstractTC=fileNameOfAbstractTC;
		this.N_max=N_max;
		this.t_initial=t_initial;
		this.SuiteIt_max=SuiteIt_max; 
		this.TcIt_max=TcIt_max;
		this.typeOfFitness=typeOfFitness;
	}
  
	//body
	

	public Vector<eu.fittest.eventSequenceGenerator.data.Path>  run(){
												
		if ((typeOfCoverage.equals("coverage"))||(typeOfCoverage.equals("xLeastFrequentEven"))){
			Hashtable<Long,Double> event_probabilityInitial=intialProbability();
			paths=pathsGen.generateTraces_withProbabilities(event_probabilityInitial, fsm_initial, maxLengthGeneratedTraces, folderNewEventSequences+System.getProperty("file.separator"), fileNameOfAbstractTC,typeOfCoverage);
		}
		else if (typeOfCoverage.equals("coverage_uniform")){
			Hashtable<Long,Double> event_probabilityInitial=intialUniformProbability();
			paths=pathsGen.generateTraces_withProbabilities(event_probabilityInitial, fsm_initial, maxLengthGeneratedTraces, folderNewEventSequences+System.getProperty("file.separator"), fileNameOfAbstractTC,typeOfCoverage);
		}
		else if (typeOfCoverage.equals("coverage_uniform_fromFSM")){
			Hashtable<Long,Double> event_probabilityInitial=intialUniformProbability_fromFSMflat(folderOriginalFSM);
			paths=pathsGen.generateTraces_withProbabilities(event_probabilityInitial, fsm_initial, maxLengthGeneratedTraces, folderNewEventSequences+System.getProperty("file.separator"), fileNameOfAbstractTC,typeOfCoverage);
		}
		else if  ((typeOfCoverage.equals("visit"))){
			paths=bfv.run(folderOriginalFSM,folderNewEventSequences, fileNameOfAbstractTC);
		}
		else if  ((typeOfCoverage.equals("visitWithLoop"))){
			paths=bfvwl.run(folderOriginalFSM,folderNewEventSequences, fileNameOfAbstractTC);	
		}
		else if  ((typeOfCoverage.equals("visitWithGlobalLoop"))){
			paths=bfvwgl.run(folderOriginalFSM,folderNewEventSequences, fileNameOfAbstractTC);
		}
		
		else if  ((typeOfCoverage.equals("sequences_maxK"))){
			paths=seq.run_allSequences_maxK(folderOriginalFSM,folderNewEventSequences+"_"+(k)+"_"+typeOfVisit,  fileNameOfAbstractTC, k, typeOfVisit);
		}
		else if  ((typeOfCoverage.equals("sequences_K"))){
			paths=seq.run_allSequences_K(true,folderOriginalFSM,folderNewEventSequences+"_"+(k)+"_"+typeOfVisit, fileNameOfAbstractTC, k, typeOfVisit);
		}
		else if  ((typeOfCoverage.equals("sem_maxK"))){
			paths=sem.run_allSequences_maxK(folderOriginalFSM,folderNewEventSequences+"_"+(k)+"_"+typeOfVisit, fileNameOfAbstractTC, k, typeOfVisit);
		}
		else if  ((typeOfCoverage.equals("sem_K"))){
			paths=sem.run_allSequences_K(true,folderOriginalFSM,folderNewEventSequences+"_"+(k)+"_"+typeOfVisit, fileNameOfAbstractTC, k, typeOfVisit);
		}	
		else if  ((typeOfCoverage.equals("SEMsextractor_onlyLastEvent_maxK"))){
			paths=sem_onlylastevent.run_allSequences_maxK(folderOriginalFSM,folderNewEventSequences+"_"+(k)+"_"+typeOfVisit, fileNameOfAbstractTC, k, typeOfVisit);
		}
		else if  ((typeOfCoverage.equals("SEMsextractor_onlyLastEvent_K"))){
			paths=sem_onlylastevent.run_allSequences_K(true,folderOriginalFSM,folderNewEventSequences+"_"+(k)+"_"+typeOfVisit, fileNameOfAbstractTC, k, typeOfVisit);
		}
		else if  ((typeOfCoverage.equals("alt_maxK"))){
			paths=alt.run_allSequences_maxK(folderOriginalFSM,folderNewEventSequences+"_"+(k)+"_"+typeOfVisit, fileNameOfAbstractTC, k, typeOfVisit);
		}
		else if  ((typeOfCoverage.equals("alt_K"))){
			paths=alt.run_allSequences_K(true,folderOriginalFSM,folderNewEventSequences+"_"+(k)+"_"+typeOfVisit, fileNameOfAbstractTC, k, typeOfVisit);
		}
		else if  ((typeOfCoverage.equals("diversity"))){
			paths=sa.run(N_max, folderOriginalFSM, folderNewEventSequences+"_"+(k)+"_"+typeOfFitness, fileNameOfAbstractTC, k, t_initial, SuiteIt_max, TcIt_max, typeOfFitness);
		}
		else if  ((typeOfCoverage.equals("diversity_onlyLastEvent_"))){
			paths=sa_onlyLastEvent.run(N_max, folderOriginalFSM, folderNewEventSequences+"_"+(k)+"_"+typeOfFitness, fileNameOfAbstractTC, k, t_initial, SuiteIt_max, TcIt_max, typeOfFitness);
		}
		
		return paths;
	}
	
	
	
	
	private Hashtable<Long,Double> intialProbability(){
		
		FSMmanager fsmmanager=new FSMmanager();
		File[] filelist=fsmmanager.getFilelist(folderTraces, 1);
		
		fsm_initial=fsmmanager.generateFSM(filelist, false, 0);
		
		transitionMatrix=fillFrequencyBasedTransitionMatrix(fsmmanager); //if (!uniformTransitionMatrix) 
		
		transitions_initial=new Transitions();
		for (int i = 0; i < fsm_initial.transitions.size(); i++) {
			transitions_initial.addTransition(fsm_initial.transitions.getTransitions().get(i));
		}
		
		Hashtable<Long,Double> event_probability=GetAndConvertFormatOfEventProbability(fsmmanager);
				
		return event_probability;
				
	}
	
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
	
   private Hashtable<Long,Double> intialUniformProbability(){
		
		FSMmanager fsmmanager=new FSMmanager();
		File[] filelist=fsmmanager.getFilelist(folderTraces, 1);
		
		fsm_initial=fsmmanager.generateFSM(filelist, false, 0);
		
		transitionMatrix=fillUniformTransitionMatrix(fsmmanager);
		//transitionMatrix=fillFrequencyBasedTransitionMatrix(fsmmanager); //if (!uniformTransitionMatrix) 
		
		transitions_initial=new Transitions();
		for (int i = 0; i < fsm_initial.transitions.size(); i++) {
			transitions_initial.addTransition(fsm_initial.transitions.getTransitions().get(i));
		}
		
		Hashtable<Long,Double> event_probability=GetAndConvertFormatOfEventProbability(fsmmanager);
				
		return event_probability;
				
	}
	
   
   public Hashtable<Long,Double> intialUniformProbability_fromFSMflat(String originalFsmPath){
 	  eu.fittest.eventSequenceGenerator.data.FSM fsmflat = new eu.fittest.eventSequenceGenerator.data.FSM(originalFsmPath);
 	  			
 		fsm_initial=generateFSM_fromFSMflat(fsmflat);
 			
 		transitionMatrix=fillFrequencyBasedTransitionMatrix(fsm_initial); //if (!uniformTransitionMatrix) 
 		
 		transitions_initial=new Transitions();
 		for (int i = 0; i < fsm_initial.transitions.size(); i++) {
 			transitions_initial.addTransition(fsm_initial.transitions.getTransitions().get(i));
 		}
 		
 		Hashtable<Long,Double> event_probability=GetAndConvertFormatOfEventProbability(fsm_initial);
 				
 		return event_probability;
 	  
   }
	
	private Hashtable<Long,Double> GetAndConvertFormatOfEventProbability(FSMmanager fsmmanager){
		return GetAndConvertFormatOfEventProbability(fsmmanager.fsmAllInOne);
	}
    private Hashtable<Long,Double> GetAndConvertFormatOfEventProbability(Fsm fsmFromLogs){
		
		double[] event_probs=getProbabilityForEvent(fsmFromLogs);
		
		Hashtable<Long,Double> event_probability=new Hashtable<Long,Double>();
		
		for (int e = 0; e < fsmFromLogs.transitions.getTransitions().size(); e++) {
			event_probability.put(new Long(fsmFromLogs.transitions.getTransitions().get(e).getidTransition()), 
					new Double(event_probs[e]));
		}
	
		return event_probability;
	}
	
	private double[] getProbabilityForEvent(Fsm fsmFromLogs){
		double[] p=new double[fsmFromLogs.transitions.getTransitions().size()];
		Vector<Transition> outgoing_sourceSt_E;
		
		for (int e = 0; e < fsmFromLogs.transitions.getTransitions().size(); e++) {
			outgoing_sourceSt_E=fsmFromLogs.transitions.getTransitionsBy_SourceId(fsmFromLogs.transitions.getTransitions().get(e).getIdStateSource());
			p[e]=1/(new Double(outgoing_sourceSt_E.size()).doubleValue());
		}
		return p;
	}
	private double[] getProbabilityForEvent(FSMmanager fsmmanager){
		return getProbabilityForEvent(fsmmanager.fsmAllInOne);
	}

  private double[][] fillFrequencyBasedTransitionMatrix(Fsm fsmFromLogs){
	  
	  double[][] transitionMatrix=new double[fsmFromLogs.states.size()][fsmFromLogs.states.size()];
		
		State target;
		Vector<Transition> vt;
		
		for (int s_r = 0; s_r < fsmFromLogs.states.size(); s_r++) {
			State source=fsmFromLogs.states.getStates().get(s_r);
			Vector<Transition> outsTrans=fsmFromLogs.transitions.getTransitionsBy_SourceId(source.getId());
			Vector<State> tstate=new Vector<State>();
			for (Transition transition : outsTrans) {
				tstate.add(fsmFromLogs.states.getStateById(transition.getIdStateTarget()));
			}
			double freqSum_fromSource=0;
			for (State stateTarget : tstate) {
				vt=fsmFromLogs.transitions.getTransitionsBySourceId_and_TargetId(source.getId(), stateTarget.getId());
				for (Transition t : vt) {
					freqSum_fromSource=freqSum_fromSource+t.getExcFreqTransition();
				}
			}
			
			double freqSumXcuttentEvent=0;
			for (int s_c = 0; s_c < fsmFromLogs.states.size(); s_c++) {
				transitionMatrix[s_r][s_c]=0;
				target=fsmFromLogs.states.getStates().get(s_c);
				if (contains(tstate,target)){
					vt=fsmFromLogs.transitions.getTransitionsBySourceId_and_TargetId(source.getId(), target.getId());
					for (Transition t : vt) {
						freqSumXcuttentEvent=freqSumXcuttentEvent+t.getExcFreqTransition();
					}
					transitionMatrix[s_r][s_c]= freqSumXcuttentEvent/ freqSum_fromSource;
				}
				
			}
		}		
	  return transitionMatrix;
	  
  }
	  
  private double[][] fillFrequencyBasedTransitionMatrix(FSMmanager fsmmanager){
	  
	  return fillFrequencyBasedTransitionMatrix(fsmmanager.fsmAllInOne);
	  
	   /*
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
		*/
	}

  private boolean contains(Vector<State> tstate,State s){
		for (State state : tstate) {
			if (state.equals(s)) return true;
		}
		return false;
	}
  

  
  
  public Fsm generateFSM_fromFSMflat(eu.fittest.eventSequenceGenerator.data.FSM fsmflat){
	  Fsm f=new Fsm();
	  
	  Set<Edge> visited = new HashSet<Edge>();
	  	  
	  for (Iterator<Node> it= fsmflat.getNodes().iterator(); it.hasNext(); ) {
		  	Node node = it.next();
		  	
		  	for (Edge e: node.getSucc()) {
				if (!visited.contains(e)) {
					 Fsm f1=visiting(node,f);
					 f=f1.clone();
				}
				visited.add(e);
			}
	  } 
	  
	  //eu.fittest.eventSequenceGenerator.data.Node startNode = fsmflat.getStartNode();
	  //f=visiting(startNode,f);
	
	  return f.clone();
  }
  
  public Fsm visiting(eu.fittest.eventSequenceGenerator.data.Node node,Fsm f){
	  for (Edge e: node.getSucc()) {
			//System.out.println("edge+="+e.getEvent());
			f.addItemToStates(new String[]{node.getLabel()}, e.getEvent(), new String[]{e.getTarget().getLabel()}, e.execFreq);
		}
	  return f;
  }
  
  
}
