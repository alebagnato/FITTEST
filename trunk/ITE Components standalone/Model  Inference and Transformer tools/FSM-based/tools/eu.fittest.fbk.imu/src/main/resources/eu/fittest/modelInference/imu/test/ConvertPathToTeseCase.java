package eu.fittest.modelInference.imu.test;

import eu.fittest.modelInference. imu.modelAlgs.Path;
import eu.fittest.modelInference.fsmInference.fsm.Transition;
import eu.fittest.modelInference.fsmInference.fsm.Transitions;

/**
 * Convert path of the FSM into TCS
 * 
 * @author Alessandro Marchetto
 *
 */
public class ConvertPathToTeseCase {

	public static TestCase convertP2TC(Path p, Transitions fsmts){
		TestCase tc;
		
		Transition t;
		
		tc=new TestCase();
		
		for (int i = 0; i < p.size(); i++) {
			try{
				t=fsmts.getTransitionById(p.getPathTransitionsVector().get(i));
				tc.addEvent(t);
				
				
				
			}catch(Exception e){
				System.out.println("Error in ConvertPathToTeseCase.convertP2TC "+i+" "+p.getPathVector().get(i));
				}
		}
		
		return tc;
	}
	
}
