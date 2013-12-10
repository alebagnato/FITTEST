package eu.fittest.fbk.efsm2ct.log2efsm.logconv.model;

/**
 * Models an event as logged by an instrumented SUT
 * 
 * @author tiella
 * 
 */

public class Event {

	private Instance recordEvent;
	private Instance appAbstractState;

	public Instance getRecordEvent() {
		return recordEvent;
	}

	public void setRecordEvent(Instance recordEvent) {
		this.recordEvent = recordEvent;
	}

	public Instance getAppAbstractState() {
		return appAbstractState;
	}

	public void setAppAbstractState(Instance appAbstractState) {
		this.appAbstractState = appAbstractState;
	}

	public void dump(String tabs) {

		System.out.println(tabs + "Event");

		tabs += "\t";

		recordEvent.dump(tabs);
		appAbstractState.dump(tabs);

	}

}
