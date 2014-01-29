package eu.fittest.modelInference.fsmInference.fsm;

import java.util.*;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Unique {
	private static Unique instance = null;

	Vector<Long> uniqueIdStates;
	Vector<Long> uniqueIdTransitions;

	private Unique() {
	}

	public static Unique getInstace() {
		if (instance == null) {
			instance = new Unique();
			instance.uniqueIdStates = new Vector<Long>();
			instance.uniqueIdTransitions = new Vector<Long>();
		}
		return instance;
	}

	public boolean iscontainedState(long valueToSearch) {
		for (int i = 0; i < instance.uniqueIdStates.size(); i++) {
			if (instance.uniqueIdStates.get(i).intValue() == valueToSearch) {
				return true;
			}
		}
		return false;
	}

	public boolean iscontainedTransitions(long valueToSearch) {
		for (int i = 0; i < instance.uniqueIdTransitions.size(); i++) {
			if (instance.uniqueIdTransitions.get(i).intValue() == valueToSearch) {
				return true;
			}
		}
		return false;
	}

	public void reset() {
		instance = new Unique();
		instance.uniqueIdStates = new Vector<Long>();
		instance.uniqueIdTransitions = new Vector<Long>();
	}

	public Vector<Long> getUniqueIdStates() {
		return instance.uniqueIdStates;
	}

	public void setUniqueIdStates(Vector<Long> uniqueIdStates) {
		instance.uniqueIdStates = uniqueIdStates;
	}

	public void addToUniqueIdStates(long uniqueIdStates) {
		instance.uniqueIdStates.add(new Long(uniqueIdStates));
	}

	public void addToUniqueIdTransitions(long uniqueIdTransitions) {
		instance.uniqueIdTransitions.add(new Long(uniqueIdTransitions));
	}

	public Vector<Long> getUniqueIdTransitions() {
		return instance.uniqueIdTransitions;
	}

	public void setUniqueIdTransitions(Vector<Long> uniqueIdTransitions) {
		instance.uniqueIdTransitions = uniqueIdTransitions;
	}

}
