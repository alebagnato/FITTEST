package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import java.util.regex.Pattern;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Event;

public class TargetMatchingRegExpEventCondition implements EventCondition {
	
	private Pattern pattern;
	
	
	public TargetMatchingRegExpEventCondition(String patternString) {
		super();
		this.pattern = Pattern.compile(patternString);
	}


	@Override
	public boolean accept(Event ev) {
		
		String target = (String)ev.getRecordEvent().get("targetID").getValue();
		String type = (String)ev.getRecordEvent().get("type").getValue();
		
		return !pattern.matcher(target+"/"+type).matches();

	}

}
