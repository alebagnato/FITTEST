package eu.fittest.modelInference.imu.modelAlgs;

import eu.fittest.modelInference.fsmInference.fsm.*;

import eu.fittest.modelInference.fsmInference.manager.FSMmanager;

import java.util.*;
 
/**
 * Path extraction form FSM
 * 
 * @author Alessandro Marchetto
 *
 */
public class PathExtraction {
	Transitions fsmts;
	FSMmanager fsmmanager;
	Vector<Path> paths;
	
	public Vector<Path> getExtractedPaths(){
		return paths;
	}
	int MAX_LOOP=1;
	
	
	public long[] setParams(FSMmanager fsmmanager,int MAX_LOOP){
		this.fsmmanager=fsmmanager;
		this.fsmts=fsmmanager.fsmAllInOne.transitions;
		this.MAX_LOOP=MAX_LOOP;
	
		paths=new Vector<Path>();
		
		long rootStateId=getRootState();
		long endStateId=getEndState();
		
		long[] ret=new long[2];
		ret[0]=rootStateId;
		ret[1]=endStateId;
		
		return ret;
	}
	
	public void pathExtraction(Transitions transitions,int MAX_LOOP){
		this.fsmts=transitions;
		this.MAX_LOOP=MAX_LOOP;
	
		paths=new Vector<Path>();
		
		long rootStateId=getRootState();
		long endStateId=getEndState();
		if ((rootStateId>-1)&&(endStateId>-1)){
			computeFSMPaths(rootStateId,endStateId);
			cleanerPaths();
		}
	}
		
	public void pathExtraction(FSMmanager fsmmanager,int MAX_LOOP){
		this.fsmmanager=fsmmanager;
		pathExtraction(fsmmanager.fsmAllInOne.transitions,MAX_LOOP);
	}
	
	
	void printPaths(Vector<Path> vps){
		Path tmpP;
		Vector<Long> tmpStates;
		Vector<Long> tmpEvents;
		
		System.out.println("Number of TestCases="+vps.size());
		
		for (int i = 0; i < vps.size(); i++) {
			 tmpP=vps.get(i);
			 tmpStates=tmpP.getPathVector();
			 tmpEvents=tmpP.getPathTransitionsVector();
			 System.out.println("path => "+tmpEvents.toString());
			
		}
	}
	
	
	long getRootState(){
		long stateId=-1;
		for (int i = 0; i < fsmts.size(); i++) {
			stateId=fsmts.getTransitions().get(i).getIdStateSource();
			if (getIngoingTransitions(stateId).size()==0) {
			
				return stateId;
			}
		}
		return -1;
	}
	
	long getEndState(){
		long stateId=-1;
		for (int i = 0; i < fsmts.size(); i++) {
			stateId=fsmts.getTransitions().get(i).getIdStateTarget();
			if (getOngoingTransitions(stateId).size()==0) {
				return stateId;
			}
		}
		return -1;
	}
	

	
	Vector<Path> cleanerPaths(){
		Path tmpP;
		Vector<Path> pathsUnique=new Vector<Path>();
		for (int i = 0; i < paths.size(); i++) {
			tmpP=paths.get(i);
			if (!iscontained(pathsUnique,tmpP)){
				pathsUnique.add(tmpP);
			}
		}
		return pathsUnique;
	}
	
	boolean iscontained(Vector<Path> vp,Path p){
		Path tmpP;
		if (p==null) return false;

		for (int i = 0; i < vp.size(); i++) {
			 tmpP=vp.get(i);
			 if (tmpP.equals(p)) return true;
		}
		return false;
	}
	
	
	
	public Vector<Path> computeFSMPaths(long fsmSourceStateId, long fsmTargetStateId) {
		if (fsmts.getTransitions().size() > 0){
			visit(fsmSourceStateId, fsmTargetStateId, -1, new Path());
			 
		}
		return paths;
	}
	
	void visit(long fsmSourceStateId, long fsmTargetStateId, long edgeId, Path path){
		
		long sonId; long edgeId2;

		if (fsmSourceStateId == fsmTargetStateId) {
			if (edgeId>-1) {
				path.getPathVector().add(new Long(fsmSourceStateId));
				path.getPathTransitionsVector().add(new Long(edgeId));
			
				Path p=new Path(new Vector(path.getPathVector()), new Vector(path.getPathTransitionsVector()));
				if (!iscontained(paths,p)){
					paths.add(p);
				}
			
				path.getPathVector().remove(path.getPathVector().size() - 1);
				path.getPathTransitionsVector().remove(path.getPathTransitionsVector().size() - 1);
				}
			return;
			
		}

		if (countOccurencies(fsmSourceStateId, path) > MAX_LOOP) { 
			return;
		}

		if (edgeId>-1) {
			path.getPathVector().add(new Long(fsmSourceStateId));
			path.getPathTransitionsVector().add(new Long(edgeId));
		}
		
		
		for (int i = 0; i < getOngoingTransitions(fsmSourceStateId).size(); i++) {
			edgeId2= getOngoingTransitions(fsmSourceStateId).get(i).getidTransition();
			sonId = getOngoingTransitions(fsmSourceStateId).get(i).getIdStateTarget();
			visit(sonId, fsmTargetStateId, edgeId2, path);
		}
		
		if (edgeId>-1) {
			path.getPathVector().remove(path.getPathVector().size() - 1);
			path.getPathTransitionsVector().remove(path.getPathTransitionsVector().size() - 1);
		}
		
	}
	
	
	int countOccurencies(long fsmSourceStateId, Path path){
		int count=0;
		for (int i = 0; i < path.getPathVector().size(); i++) {
			if (path.getPathVector().get(i).longValue()==fsmSourceStateId) {
				count++;
			}
		}
		return count;
	}
	
	
	Vector<Transition> getOngoingTransitions(long stateId){
		return fsmts.getTransitionsBy_SourceId(stateId);
	}
			
	
		

	
	Vector<Transition> getIngoingTransitions(long stateId){
		return fsmts.getTransitionsBy_TargetId(stateId);
	}
 

	
}