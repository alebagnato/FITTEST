package eu.fittest.tloglib.event;

import java.util.*;
import eu.fittest.tloglib.error.* ;

public class EventList {

	public List<Event> eventlist ;
	
	/**
	 * This will parse the given eventlog (a text file) to construct an
	 * internal representation of it (which is a list of Event-objects).
	 */
	public EventList(String eventlog) {
		eventlist = new ArrayList<Event>() ;
		parse(eventlog) ;
	}

	private void parse(String s){
		String[] z = s.split(":") ;
		for (int i=1; i<z.length-2; i=i+3) {
			int time = Integer.parseInt(z[i]) ;
			int code = Integer.parseInt(z[i+1]) ;
			String name = z[i+2] ;
			Event e = null ;
			switch (code) {
			case DynamicValueEvent.CODE : e = new DynamicValueEvent(time,name) ; break ;
			case ExceptionEvent.CODE : e = new ExceptionEvent(time,name) ; break ;
			case UntracedCall.CODE : e = new UntracedCall(time,name) ; break ;
			case FinallyEvent.CODE : e = new FinallyEvent(time) ; break ;
			default : throw new LogDecodingError("Failure to parse eventlog; unknown code: " + code) ;
			}
			eventlist.add(e) ;
		}
	}
		
	/**
	 * Check if the top event has matured.
	 */
	public boolean isMature(int prc){
		if (eventlist.isEmpty()) return false ;
		Event e = eventlist.get(0) ;
		if (prc < e.time) return false ;
		if (prc > e.time) throw new LogDecodingError("Progress counting error, prc=" + prc 
				+ ", but top event has maturity time=" + e.time) ;
		return true ;
	}
	
	/**
	 * Pop and replay the top event.
	 */
	public void replay() throws Exception {
		Event e = eventlist.get(0) ;
		eventlist.remove(0) ;
		if (e instanceof FinallyEvent) {
			FinallyEvent f = (FinallyEvent) e ;
			f.replay(this) ;
		}
		else e.replay() ;
	}
			
	public static void main(String[] args) {
		String example = ":10:1:Bla.Bla.Exception:12:2:99900:45:4::99:3:fooDEC" ;
		EventList z = new EventList(example) ;
		for (Event e : z.eventlist) {
			System.out.println(e.serialize()) ;
		}
	}
	
}
