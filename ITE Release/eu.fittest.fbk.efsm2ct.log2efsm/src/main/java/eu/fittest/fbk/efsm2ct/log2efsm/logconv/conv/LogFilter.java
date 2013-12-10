package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import java.util.ArrayList;
import java.util.List;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Event;

public class LogFilter {

	private List<EventCondition> conditions = new ArrayList<EventCondition>();
	
	public void addCondition(EventCondition c) {
		conditions.add(c);
	}
	
	public List<Event> filter(List<Event> input) {
		
		List<Event> res = new ArrayList<Event>();
		
		for (Event e : input) {
			
			boolean accepted = true;
			
			for (EventCondition c : conditions) {
				
				if (!c.accept(e)) {
					accepted = false;
					break;
				}
				
			}
			
			if (accepted) {
				res.add(e);
			}
			
		}
	
		return res;
	}
	
}
