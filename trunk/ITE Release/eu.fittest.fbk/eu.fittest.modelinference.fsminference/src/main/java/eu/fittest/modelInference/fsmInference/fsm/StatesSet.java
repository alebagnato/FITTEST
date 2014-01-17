package eu.fittest.modelInference.fsmInference.fsm;

import java.util.*;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class StatesSet {

	public Vector<Fsm> statesSet;
	public int countEventsInLogs = 0;

	public StatesSet() {
		statesSet = new Vector<Fsm>();
	}

	public Vector<Fsm> getAllStatesSet() {
		return statesSet;
	}

	public void addStates(Fsm statesToadd) {
		Fsm f = statesToadd.clone();

		statesSet.add(f);
	}

	public void addToCountEventsInLogs(int num) {
		countEventsInLogs = countEventsInLogs + num;
	}

	public void setCountEventsInLogs(int num) {
		countEventsInLogs = num;
	}

	public int getCountEventsInLogs() {
		return countEventsInLogs;
	}
}
