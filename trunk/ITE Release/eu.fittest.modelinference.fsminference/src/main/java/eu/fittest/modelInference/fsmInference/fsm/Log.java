package eu.fittest.modelInference.fsmInference.fsm;

import java.util.*;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Log {

	public Vector<Line> lines;

	public Log() {
		lines = new Vector<Line>();
	}

	public Vector<Line> getAllLines() {
		return lines;
	}

	public int size() {
		return lines.size();
	}

	public void addLine(Line line) {
		lines.add(line);
	}

	public Vector<String> getEvents() {
		Line line = null;
		Vector<String> listOfEvents = new Vector<String>();
		for (int i = 0; i < lines.size(); i++) {
			line = lines.get(i);
			if (!listOfEvents.contains(line.getEvent())) {
				listOfEvents.add(line.getEvent());
			}
		}
		return listOfEvents;

	}

	public Vector<String[]> getStates() {
		Line line = null;
		Vector<String[]> listOfStates = new Vector<String[]>();
		for (int i = 0; i < lines.size(); i++) {
			line = lines.get(i);
			if (!listOfStates.contains(line.getStatus())) {
				listOfStates.add(line.getStatus());
			}
		}
		return listOfStates;

	}

}
