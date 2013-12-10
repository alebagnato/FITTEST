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

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Line {
	public String fileName;
	public String event;
	public String eventName;
	public String[] status;

	public Line(String fileName, String event, String eventName, String[] status) {
		this.fileName = fileName;
		this.event = event;
		this.eventName = eventName;
		this.status = new String[status.length];
		this.status = status;
	}

	public String getfileName() {
		return fileName;
	}

	public String getEvent() {
		return event;
	}

	public String getEventName() {
		return eventName;
	}

	public String[] getStatus() {
		return status;

	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (this == null)
			return false;
		if (!(o instanceof Line))
			return false;
		boolean eq = false;
		eq = ((Line) o).getfileName().equals(this.fileName);
		eq = ((Line) o).getEvent().equals(this.event);
		eq = ((Line) o).getEventName().equals(this.eventName);
		eq = ((Line) o).getStatus().equals(this.status);
		return eq;
	}

	public boolean equalsStatus(Object o) {
		if (this == o)
			return true;
		if (this == null)
			return false;
		if (!(o instanceof Line))
			return false;
		return ((Line) o).getStatus().equals(this.status);
	}

}
