package eu.fittest.modelInference.imu.modelAlgs;

import eu.fittest.modelInference.fsmInference.fsm.Fsm;

import eu.fittest.modelInference.fsmInference.fsm.Transition;
import eu.fittest.modelInference.fsmInference.fsm.Transitions;

import java.util.*;

import eu.fittest.modelInference.imu.utility.Utility;
import eu.fittest.modelInference.imu.config.Config_Launcher;


/**
 * Probabilistic extraction of paths
 * 
 * @author Alessandro Marchetto
 *
 */
public class Probabilistic_PathExtraction {
	Hashtable<Long,Double> event_probability=null;
	int maxLength_SinglePath=20;
	Utility utils=new Utility();
	Fsm fsmall;
	Transitions fsmts;
	Config_Launcher cl=Config_Launcher.getInstace();
	Vector<Path> paths;
	
	String typeOfCoverage="coverage"; //coverage, xLeastFrequentEven
	
	public void setTypeOfCoverage(String typeOfCoverage){
		this.typeOfCoverage=typeOfCoverage;
	}
	
	void setTo0(){
		coverageMeasure=new Hashtable<Long,Integer>();
		Set<Long> set = event_probability.keySet();
		 long key_eId;
		 Iterator<Long> itr = set.iterator();
		 
		 while (itr.hasNext()) {
			 key_eId= (itr.next()).longValue();
			 coverageMeasure.put(new Long(key_eId), new Integer(0));
		 }
	}
	
	void updateCoverage(Path path){
		int tmpo=0;
		if (path!=null){
			for (int i = 0; i < path.getPathTransitionsVector().size(); i++) {
				tmpo=coverageMeasure.get(path.getPathTransitionsVector().get(i)).intValue();
				tmpo++;
				coverageMeasure.put(path.getPathTransitionsVector().get(i), new Integer(tmpo));
				}
			
		}
	}
	
	Hashtable<Long,Integer> coverageMeasure;
	
	boolean checkCoverage_xAllEvents(){
		if (coverageMeasure==null) return false;
		if (coverageMeasure.size()==0)return false;
		
		boolean triesToCheck=false;
		
		Set<Long> set = coverageMeasure.keySet();
		 long key_eId;
		 int value_eProb;
		 Iterator<Long> itr = set.iterator();
		 while (itr.hasNext()) {
			 key_eId= (itr.next()).longValue();
			 value_eProb=(coverageMeasure.get(new Long(key_eId))).intValue();
			 if (value_eProb<1) {
				 triesToCheck=false;
				 return false;
			 }
			 else triesToCheck=true;
		 }
		 return triesToCheck;
	}
	
	boolean checkCoverage_xEvent(long idEventToBeChanged){
		if (coverageMeasure==null) return false;
		if (coverageMeasure.size()==0)return false;
		
		try{
			int value_eProb=(coverageMeasure.get(new Long(idEventToBeChanged))).intValue();
			if (value_eProb<cl.evaluationA_fsmcoverageTimes_PerEvent) {
			
			 return false;
		 }else return true;
		
		}catch(Exception e){
			return checkCoverage_xLeastFrequentEvent();
		}
	}
	
	boolean checkCoverage_xLeastFrequentEvent(){
		if (coverageMeasure==null) return false;
		if (coverageMeasure.size()==0)return false;
				
		long least=getLeastFrequentEvent();
		if (least==-1) return false;
		
		int value_eProb=(coverageMeasure.get(new Long(least))).intValue();
		 if (value_eProb<cl.evaluationA_fsmcoverageTimes_PerEvent) {
			  return false;
		 }else {
			 return true;
		 }
		
		 
	}
	
	public Path pathExtraction_withProbability(Fsm fsmall,Hashtable<Long,Double> event_probability,int numMaxPaths, int maxLength_SinglePath){
		this.fsmall=fsmall;
		this.event_probability=event_probability;
		this.maxLength_SinglePath=maxLength_SinglePath;
		this.fsmts=fsmall.transitions;
			
		long rootStateId=getRootState();
		
		if (rootStateId>-1){
			
			Path p_tmp;
			
			int i=0;
			
			p_tmp=extractPath_withProbability_MinimlLength(rootStateId,maxLength_SinglePath);
							
			while ((i<100)&&(p_tmp.size()<maxLength_SinglePath)){
				p_tmp=extractPath_withProbability_MinimlLength(rootStateId,maxLength_SinglePath);
				i++;
			}
			
			
			return p_tmp;

		}
		
		return null;
	}
	
	public Vector<Path> pathsExtraction_withProbability(Fsm fsmall,Hashtable<Long,Double> event_probability, int maxLength_SinglePath,long idEventToBeChanged){
		this.fsmall=fsmall;
		this.event_probability=event_probability;
		this.maxLength_SinglePath=maxLength_SinglePath;
		this.fsmts=fsmall.transitions;
		setTo0();
		
		paths=new Vector<Path>();
		
		
		long rootStateId=getRootState();
		
		if (rootStateId>-1){
			
			Path p_tmp;
			
			if (typeOfCoverage.equals("xLeastFrequentEven")){
			
			/*
			 *  // for using other coverage criteria select one of the following:
			 * while((i<cl.evaluationA_max)&&(checkCoverage()==false)){
			 * while((checkCoverage_xEvent(idEventToBeChanged)==false)||(checkCoverage_xAllEvents()==false)){
			 * while((checkCoverage_xLeastFrequentEvent()==false)&&(checkCoverage_xAllEvents()==false)){
			 * while(checkCoverage_xLeastFrequentEvent()==false){
			 * while(checkCoverage_xEvent(idEventToBeChanged)==false){
			 * while((checkCoverage_xAllEvents()==false)){
			 */
			 
		     while(checkCoverage_xLeastFrequentEvent()==false){ //funziona ORG
			//while((checkCoverage_xLeastFrequentEvent()==false)&&(checkCoverage_xAllEvents()==false)){
			

					p_tmp=extractPath_withProbability(rootStateId);	
									
					//if (!iscontained(paths,p_tmp)){	//to collect only unique paths
						updateCoverage(p_tmp);
						paths.add(p_tmp);	
					//}
				
					
	
				}
			}else if (typeOfCoverage.equals("coverage")){
				while(checkCoverage_xAllEvents()==false){
						p_tmp=extractPath_withProbability(rootStateId);	
						updateCoverage(p_tmp);
						paths.add(p_tmp);	
						}
				
				}
			else {
				 while(checkCoverage_xLeastFrequentEvent()==false){ //funziona ORG
					 p_tmp=extractPath_withProbability(rootStateId);	
					 updateCoverage(p_tmp);
					 paths.add(p_tmp);	
				 	}
				}
			
			
			}
					
		return paths;
	}
	
	Path extractPath_withProbability(long fsmSourceStateId){
		Path path=new Path();
		long nextSourceStateID=fsmSourceStateId;
		long nexEventID=0;				
		int i=0;
		
		
		
		int maxI=utils.randomInt(maxLength_SinglePath, true);
		
		path.getPathVector().add(new Long(nextSourceStateID));
	
		while((nexEventID>-1)&&(i<maxI)){
							
			nexEventID=geNextEvent(nextSourceStateID);
			
			if (nexEventID>-1){
				path.getPathTransitionsVector().add(nexEventID);
				nextSourceStateID=fsmts.getTransitionById(nexEventID).getIdStateTarget();
				path.getPathVector().add(new Long(nextSourceStateID));
			}
			
			i++;
		}
		
		
		
		return path;
	}
	
	Path extractPath_withProbability_MinimlLength(long fsmSourceStateId,int minLength_SinglePath){
		Path path=new Path();
		long nextSourceStateID=fsmSourceStateId;
		long nexEventID=0;				
		
		path.getPathVector().add(new Long(nextSourceStateID));
		
		while((nexEventID>-1)&&(path.size()<minLength_SinglePath)){
			
			
			nexEventID=geNextEvent(nextSourceStateID);
			
			
			if (nexEventID>-1){
				
				path.getPathTransitionsVector().add(nexEventID);
				nextSourceStateID=fsmts.getTransitionById(nexEventID).getIdStateTarget();
				path.getPathVector().add(new Long(nextSourceStateID));
				
			}
		}
		
		
		
		return path;
	}
	
	
	long geNextEvent(long fsmCurrentStateId){
		Vector<Transition> outs=getOngoingTransitions(fsmCurrentStateId);
		
		if (outs.size() == 0){
			return -1;
		}
		
		double d=utils.randomDouble(false, false);
		
		Double value_eProb_D=null;
		long idt=0; 
		double sum=0;
		double value_eProb;
		for (int i = 0; i < outs.size(); i++) {
			
			idt=outs.get(i).getidTransition();
			value_eProb_D=null;
			value_eProb_D=event_probability.get(new Long(idt));
						
			if (value_eProb_D!=null){
				value_eProb=value_eProb_D.doubleValue();
				sum=sum+value_eProb;	
				
				if (d<sum) {
						return idt;
				}
			}
		}
		
		return -1;
		
		
		
		
	}
	
	
	long getRootState(){
		for (int i = 0; i < fsmall.states.size(); i++) {
			if (fsmall.states.getStates().get(i).getStateContent().length>0){
				//if ((fsmall.states.getStates().get(i).getStateContent()[0].equals("start"))||(fsmall.states.getStates().get(i).getStateContent()[0].equals("S0"))||(fsmall.states.getStates().get(i).getStateContent()[0].equals("S1")) ) {
				if ((fsmall.states.getStates().get(i).getStateContent()[0].equals("start")))  {
					return fsmall.states.getStates().get(i).getId();
				}
			}
		}
		
		int numinput=0;
		for (int i = 0; i < fsmall.states.size(); i++) {
			numinput=0;
			for (int j = 0; j < fsmall.transitions.size(); j++) {
				if (fsmall.transitions.getTransitions().get(j).getIdStateTarget() == fsmall.states.getStates().get(i).getId()){
					numinput++; 
				}
				
			}
			if (numinput==0) return fsmall.states.getStates().get(i).getId();
		}
		
		return -1;
	}
	
	/*long getEndState(){
		for (int i = 0; i < fsmall.states.size(); i++) {
			if (fsmall.states.getStates().get(i).getStateContent().length>0){
				if (fsmall.states.getStates().get(i).getStateContent()[0].equals("end")) {
					return fsmall.states.getStates().get(i).getId();
				}
			}
		}
		
		return -1;
	}*/
	
	long getLeastFrequentEvent(){
		long least=-1;
		double min=1;
		Set<Long> set = event_probability.keySet();
		 long key_eId;double value_eProb;
		 Iterator<Long> itr = set.iterator();
		 int i=0;
		 while (itr.hasNext()) {
			 key_eId= (itr.next()).longValue();
			 value_eProb=(event_probability.get(new Long(key_eId))).doubleValue();
			 
				 if (value_eProb<min) { 
					 least=key_eId;
					 min=value_eProb;					 
				 
			 }
			 
		 }
		 return least;
		
	}

	//---------- from parent
	boolean iscontained(Vector<Path> vp,Path p){
		Path tmpP;
		if (p==null) return false;

		for (int i = 0; i < vp.size(); i++) {
			 tmpP=vp.get(i);
			 if (tmpP.equals(p)) return true;
		}
		return false;
	}
	Vector<Transition> getOngoingTransitions(long stateId){
		return fsmts.getTransitionsBy_SourceId(stateId);
	}
	
	
}

