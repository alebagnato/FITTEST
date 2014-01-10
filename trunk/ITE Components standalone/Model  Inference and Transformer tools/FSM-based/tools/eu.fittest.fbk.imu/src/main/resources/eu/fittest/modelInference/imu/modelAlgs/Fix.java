package eu.fittest.modelInference.imu.modelAlgs;

import eu.fittest.modelInference.fsmInference.manager.FSMmanager;


import java.util.Vector;

/**
 * FSM fixing
 * 
 * @author Alessandro Marchetto
 *
 */
public class Fix {
	PathExtraction pathGenerator=new PathExtraction();
	
	Vector<Long> toBeRemovedTId=null;
	Vector<Long> toBeRemovedSId=null;
	
	public Vector<Long> getTtoBeRemoved(){
		return toBeRemovedTId;
	}
	public Vector<Long> getStoBeRemoved(){
		return toBeRemovedSId;
	}
	

	public void fixModel(FSMmanager fsmmanager){
		//to be done ....
		
		Vector<Long> allFoundTId=new Vector<Long>();
		Vector<Long> allFoundSId=new Vector<Long>();
		
		Vector<Long> actualTId=new Vector<Long>();
		Vector<Long> actualSId=new Vector<Long>();
		
		toBeRemovedTId=new Vector<Long>();
		toBeRemovedSId=new Vector<Long>();
		
		 long tmp1=0;long tmp2=0;long tmp3=0;
		  
	    //found transitions and states
		pathGenerator.pathExtraction(fsmmanager, 1);
	    
		Vector<Path> paths= pathGenerator.getExtractedPaths();
	
	    
	    for (int i = 0; i < paths.size(); i++) {
	    	for (int i2 = 0; i2 < paths.get(i).size(); i2++) {
	    		
	    		tmp1=paths.get(i).getPathTransitionsVector().get(i2);
	    		if (!allFoundTId.contains(new Long(tmp1))) {
	    			allFoundTId.add(new Long(tmp1));
	    		}
	    		
	    		tmp2=fsmmanager.fsmAllInOne.transitions.getTransitionById(tmp1).getIdStateSource();
	    		tmp3=fsmmanager.fsmAllInOne.transitions.getTransitionById(tmp1).getIdStateSource();
	    		
	    		if (!allFoundSId.contains(new Long(tmp2))) allFoundSId.add(new Long(tmp2));
		    	if (!allFoundSId.contains(new Long(tmp3))) allFoundSId.add(new Long(tmp3));
	    		
	    	}
		}
	    
	    //actual id for transitions and states
	   
	    for (int i = 0; i < fsmmanager.fsmAllInOne.transitions.getTransitions().size(); i++) {
	    	
	    	tmp1=fsmmanager.fsmAllInOne.transitions.getTransitions().get(i).getidTransition(); 
	    	tmp2=fsmmanager.fsmAllInOne.transitions.getTransitions().get(i).getIdStateSource();
	    	tmp3=fsmmanager.fsmAllInOne.transitions.getTransitions().get(i).getIdStateTarget();
	    	
	    	if (!actualTId.contains(new Long(tmp1))) actualTId.add(new Long(tmp1));
	    	if (!actualSId.contains(new Long(tmp2))) actualSId.add(new Long(tmp2));
	    	if (!actualSId.contains(new Long(tmp3))) actualSId.add(new Long(tmp3));
	    	
	    }
	    
	    //identification of not existing ones
	    for (int i = 0; i < actualTId.size(); i++) {
	    	if (!allFoundTId.contains(actualTId.get(i))) {
	    		toBeRemovedTId.add(actualTId.get(i));
	    	}
		}
	    for (int i = 0; i < actualSId.size(); i++) {
	    	if (!allFoundSId.contains(actualSId.get(i))) {
	    		toBeRemovedSId.add(actualSId.get(i));
	    	}
		}
	    
	}
	
}
