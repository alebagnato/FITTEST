package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Event;

public interface EventCondition {

	boolean accept(Event ev);
	
}
