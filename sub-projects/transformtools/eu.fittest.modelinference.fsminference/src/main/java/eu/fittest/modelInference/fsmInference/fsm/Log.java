/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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
