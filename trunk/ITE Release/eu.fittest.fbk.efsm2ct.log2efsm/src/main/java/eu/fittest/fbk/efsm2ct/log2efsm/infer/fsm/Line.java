package eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm;

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
