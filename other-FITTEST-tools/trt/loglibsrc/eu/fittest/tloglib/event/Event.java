package eu.fittest.tloglib.event;

public abstract class Event {

	/**
	 * The progress counter at which time the event matures (thus to be replayed).
	 */
	public int time ;
	
	/**
	 * To replay the event.
	 */
	abstract public void replay() throws Exception ;
	
	/**
	 * To serialize the event (to be later on written to an event-log).
	 * The prc is the current value of the progress counter.
	 */
	abstract public String serialize() ;
	
}
