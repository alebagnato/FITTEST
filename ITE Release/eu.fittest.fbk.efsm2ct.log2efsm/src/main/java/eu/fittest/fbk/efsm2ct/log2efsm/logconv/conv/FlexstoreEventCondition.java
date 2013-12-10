package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Event;

public class FlexstoreEventCondition implements EventCondition {

	@Override
	public boolean accept(Event ev) {
	
		String target = (String)ev.getRecordEvent().get("targetID").getValue();
		
		return !(
				target.startsWith("IDY_cancas") || 
				target.startsWith("HID_IDY_priceSlider") ||
				target.startsWith("HID_IDY_cancas") ||
				target.startsWith("IDY_filterPanel_comboboxSeries_close") ||
				target.startsWith("IDY_filterPanel_comboboxSeries_open")
				);
		
		
	}
	
}
