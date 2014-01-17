package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

public class FlexstoreFilter extends LogFilter {

	public FlexstoreFilter() {
		super();
		addCondition(new FlexstoreEventCondition());
	}

	
	
}
