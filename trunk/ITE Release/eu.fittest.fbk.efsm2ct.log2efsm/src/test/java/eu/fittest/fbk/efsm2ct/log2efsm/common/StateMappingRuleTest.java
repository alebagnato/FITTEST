package eu.fittest.fbk.efsm2ct.log2efsm.common;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class StateMappingRuleTest {

	@Test
	public void testMatch() {

		StateMappingRule r = new StateMappingRule();

		r.setAttrNameRegex(null);
		r.setInType(null);
		r.setInPred("Integer.parseInt(v) < 10");

		boolean res = r.match("x", "int", "1");

		assertTrue(res);

	}

}
