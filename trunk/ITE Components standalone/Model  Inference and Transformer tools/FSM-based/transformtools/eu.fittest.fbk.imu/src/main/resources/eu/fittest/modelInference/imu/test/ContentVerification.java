package eu.fittest.modelInference.imu.test;

import java.util.Vector;

import eu.fittest.modelInference.fsmInference.fsm.*;

/**
 * 
 * 
 * @author Alessandro Marchetto
 *
 */
public class ContentVerification {
	
	public static boolean isEventContainedInFsm(Transitions transitions, Transition event){
		Transition t;
		for (int i = 0; i < transitions.size(); i++) {
				t=transitions.getTransitions().get(i);
				if (t.equals(event)) return true;
			
		}
		return false;
	}
	
	public static boolean isEventContainedInTCS(Vector<TestCase> tcs, Transition event){
		TestCase tc;
		for (int i = 0; i < tcs.size(); i++) {
			tc=tcs.get(i);
			if (tc.isEventContained(event)) return true;
		}
		return false;
	}
		
	
	
}
