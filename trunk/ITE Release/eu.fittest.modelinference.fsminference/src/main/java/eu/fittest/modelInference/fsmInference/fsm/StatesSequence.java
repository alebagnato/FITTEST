package eu.fittest.modelInference.fsmInference.fsm;

import java.util.*;
import java.io.*;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class StatesSequence {

	BufferedWriter outputFile = null;
	Log finalStates = new Log();
	public Fsm stateSet = new Fsm();
	Vector<Line> lines = null;

	public boolean apply(boolean createNew, String fileName, Log log,
			boolean addStartState, boolean addEndState, Line lastLine,
			int execIndex) {
		boolean executed = false;
		if (createNew)
			executed = setNewStateSet();
		executed = extractSequence(log, addStartState, addEndState, lastLine,
				execIndex);
		return executed;
	}

	public boolean setNewStateSet() {
		stateSet = new Fsm();
		return true;
	}

	public String[] getNextState(int i) {
		Line line = lines.get(i);
		return line.getStatus();

	}

	public boolean extractSequence(Log log, boolean addStartState,
			boolean addEndState, Line lastLine, int execIndex) {
		lines = log.getAllLines();

		Line line = null;
		String[] state2 = null;

		for (int i = 0; i < lines.size(); i++) {
			line = lines.get(i);

			/*
			 * if ((i==0)&&(addStartState)){ state2=new String[1];
			 * state2[0]="start"; if (i!=lines.size()-1)
			 * stateSet.addItemToStates(state2,
			 * line.getEvent()+"_"+line.getEventName(), getNextState(i+1),
			 * execIndex); else stateSet.addItemToStates(state2,
			 * line.getEvent()+"_"+line.getEventName(), lastLine.getStatus(),
			 * execIndex); //if (i!=lines.size()-1)
			 * stateSet.addItemToStates(state2, "initialization",
			 * getNextState(i+1), execIndex); //else
			 * stateSet.addItemToStates(state2, "initialization",
			 * lastLine.getStatus(), execIndex); }
			 * 
			 * else
			 */if ((i == lines.size() - 1) && (addEndState)) {
				state2 = new String[1];
				state2[0] = "end";
				line = lines.lastElement();
				stateSet.addItemToStates(line.getStatus(), line.getEvent()
						+ "_" + line.getEventName(), state2, execIndex);
			}

			else if ((i == lines.size() - 1) && (!addEndState)) {
				stateSet.addItemToStates(line.getStatus(), line.getEvent()
						+ "_" + line.getEventName(), lastLine.getStatus(),
						execIndex);
			}

			else {
				String[] currentStatus = line.getStatus();
				if ((i == 0) && (addStartState)) {
					state2 = new String[1];
					state2[0] = "start";
					stateSet.addItemToStates(state2, "_initialization",
							currentStatus, execIndex);
				}
				stateSet.addItemToStates(currentStatus, line.getEvent() + "_"
						+ line.getEventName(), getNextState(i + 1), execIndex);

			}

		}

		return true;
	}

}
