package eu.fittest.modelInference.imu.test;

import java.util.*;

import eu.fittest.modelInference.fsmInference.fsm.*;

/**
 * TC
 * 
 * @author Alessandro Marchetto
 *
 */
public class TestCase {
	Vector<Transition> tc;
	
	public TestCase(){
		tc=new Vector<Transition>();
	}
	
	public void addEvent(Transition event){
		tc.add(event.clone());
	}
	
	public Vector<Transition> getTC(){
		return  tc;
	}
	
	public Transition getEventInTC(int eventindex){
		return tc.get(eventindex);
	}
	
	public int size(){
		return tc.size();
	}
	
	public boolean isEventContained(Transition event){
		for (int i = 0; i < tc.size(); i++) {
			if (tc.get(i).equals(event)) return true;
		}
		return false;
	}
	
	public boolean isEqual(TestCase tcToBeVerified){
		boolean iseq=false;
		if (tcToBeVerified.size()!=tc.size()) return false;
		for (int i = 0; i < tc.size(); i++) {
			if (!tc.get(i).equals(tcToBeVerified.getTC().get(i))) return false;
			iseq=true;
		}
		return iseq;
	}
}
