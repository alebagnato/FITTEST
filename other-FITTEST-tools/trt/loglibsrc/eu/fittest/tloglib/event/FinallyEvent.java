package eu.fittest.tloglib.event;

import eu.fittest.tloglib.error.*;

public class FinallyEvent extends Event {

	public static final int CODE = 4 ;
	
	public FinallyEvent(int time) {
		this.time = time ; 
	}
	
	@Override
	public void replay() throws Exception { 
		// this variant won't be used
	}
	
	public void replay(EventList eventList) throws Exception {
		// Get and remove the first exception-event occuring in the list. This is used
		// in the context of the handling of "finally". When a try-catch exits due to 
		// an exception, this exception is actually delayed to be rethrown after the
		// the finally-section. The screws progress counting, so it needs a special treatment.
		ExceptionEvent r = null ;
		for (Event e : eventList.eventlist) {
			if (e instanceof ExceptionEvent) {
				r = (ExceptionEvent) e ;
				break ;
			}
		}
		if (r==null)
			throw new LogDecodingError("Error in decoding; a finally section is entered through an exception, but the exception cannot be found.") ;
		eventList.eventlist.remove(r) ;
		r.replay() ;
	}

	@Override
	public String serialize() {
		return ":" + time + ":" + CODE + ":" ;
	}

}
