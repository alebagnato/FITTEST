package eu.fittest.fbk.efsm2ct.log2efsm.common;

import org.junit.Test;

public class FlexstoreStateMappingRulesetTest {

	@Test
	public void testMap() {

		FlexstoreV3StateMappingRuleset rset = new FlexstoreV3StateMappingRuleset();

		StateMapping s = rset.map("numInShopCart", "int", "12");

		System.out.println(s);

	}

}
