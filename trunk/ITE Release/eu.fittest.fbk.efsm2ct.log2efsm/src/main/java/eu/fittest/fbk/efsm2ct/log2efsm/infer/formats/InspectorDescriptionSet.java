package eu.fittest.fbk.efsm2ct.log2efsm.infer.formats;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.Fsm;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.State;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.States;

public class InspectorDescriptionSet {

	private static List<String> ignoredInspectors = new ArrayList<String>();

	static {
		ignoredInspectors.add("start");
		ignoredInspectors.add("undef");
		ignoredInspectors.add("end");
	}

	private Map<String, InspectorDescription> inspectors = new HashMap<String, InspectorDescription>();

	public InspectorDescriptionSet(Fsm fsm) {

		States states = fsm.states;

		for (State s : states.getStates()) {

			String[] stateContent = s.getStateContent();

			for (String a : stateContent) {

				String[] exp = a.split("=");
				String[] var = exp[0].split(":");

				String inspector = var[0];

				if (!ignoredInspectors.contains(inspector) && !inspectors.containsKey(inspector)) {

					InspectorDescription inDesc = new InspectorDescription();
					inDesc.setName(inspector);
					inDesc.setType("int"); // TODO

					inspectors.put(inspector, inDesc);

				}

			}

		}
	}

	public int size() {
		return inspectors.size();
	}

	public InspectorDescription getByName(String key) {
		return inspectors.get(key);
	}

	public Set<String> keys() {
		return inspectors.keySet();
	}

	public Collection<InspectorDescription> getValues() {
		return inspectors.values();
	}

}
