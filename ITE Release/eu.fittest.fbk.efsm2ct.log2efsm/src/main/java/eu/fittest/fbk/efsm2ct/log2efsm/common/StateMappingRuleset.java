package eu.fittest.fbk.efsm2ct.log2efsm.common;

import java.util.ArrayList;
import java.util.List;

public class StateMappingRuleset {

	private List<StateMappingRule> rules = new ArrayList<StateMappingRule>();

	public void addRule(StateMappingRule r) {
		rules.add(r);
	}

	/**
	 * 
	 * @param attrName
	 * @param inType
	 * @param value
	 * @return an abstract value, null means drop the attribute
	 */

	public StateMapping map(String attrName, String inType, Object value) {

		StateMapping res = null;

		for (StateMappingRule r : rules) {

			if (r.match(attrName, inType, value)) {

				res = new StateMapping();
				res.setOutPredFmt(r.getOutPredFmt());
				res.setOutSymbol(r.getOutSymbol());
				res.setOutType(r.getOutType());

			}

		}

		return res;

	}
}
