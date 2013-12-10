package eu.fittest.ucl.autoabs.utils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Vector;

import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;

public class StateHelper {
	
	public static LinkedHashSet<ConcreteState> states = new LinkedHashSet<ConcreteState>();
	public static Vector<List<TraceEvent>> events = new Vector<List<TraceEvent>>();
	
	private static Object[] generateObjectValue(int n) {
		Object[] v = new Object[1];
		v[0] = (Object)n;
		return v;
	}
	public static int numUniqueStates() {
		return states.size();
	}
	public static void generateStatesAndEvents() {
		states.clear();
		events.clear();
		
		TraceEvent.setTraceHeader("event,N");
		states.add(new ConcreteState(generateObjectValue(-1)));
		states.add(new ConcreteState(generateObjectValue(0)));
		states.add(new ConcreteState(generateObjectValue(1)));
		states.add(new ConcreteState(generateObjectValue(2)));
		states.add(new ConcreteState(generateObjectValue(3)));
		
		List<TraceEvent> t1 = new ArrayList<TraceEvent>();
		boolean add = true;
		for(ConcreteState s : states) {
			if(((Integer)s.getValues()[0]) == -1) {
				t1.add(new TraceEvent("pay", s));
			} else {
				if(add) {
					t1.add(new TraceEvent("add", s));
					add = false;
				} else {
					t1.add(new TraceEvent("rem", s));
					add = true;
				}
			}
		}
		
		events.add(t1);
	}
}
