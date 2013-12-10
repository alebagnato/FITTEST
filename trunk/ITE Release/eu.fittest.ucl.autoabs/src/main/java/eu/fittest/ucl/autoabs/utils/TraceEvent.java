package eu.fittest.ucl.autoabs.utils;

import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;

/**
 * Class representing a trace event (i.e. row in a CSV file)
 */
public class TraceEvent {
	
	private static String header;
	
	/** event that results in this state */
	private final String eventName;
	
	/** object values for this trace event */
	private final ConcreteState state;
	
	public TraceEvent(String event, ConcreteState state) {
		this.eventName = event;
		this.state = state;
	}
	
	public static void setTraceHeader(String header) {
		TraceEvent.header = header;
	}
	
	public static String getTraceHeader() {
		return TraceEvent.header;
	}
	
	public String getEventName() {
		return this.eventName;
	}
	
	public ConcreteState getConcreteState() {
		return this.state;
	}
}
