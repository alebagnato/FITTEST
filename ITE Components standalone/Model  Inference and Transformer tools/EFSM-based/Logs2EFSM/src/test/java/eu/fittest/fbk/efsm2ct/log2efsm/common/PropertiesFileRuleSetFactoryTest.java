package eu.fittest.fbk.efsm2ct.log2efsm.common;

import java.io.File;

import org.junit.Test;
import static org.junit.Assert.*;

public class PropertiesFileRuleSetFactoryTest {

	@Test
	public void testMap() {

		try {
		
		StateMappingRuleset rset = PropertiesFileRulesetFactory.loadFromFile(new File("src/test/input/ruleset.properties"));

		StateMapping s = rset.map("numInShopCart", "int", "12");

		System.out.println(s);
		
		} catch (Exception ex) {
			fail("error: "+ex);
		}

	}

}
