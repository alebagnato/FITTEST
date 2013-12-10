package eu.fittest.fbk.efsm2ct.tools.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class TransitionParserTest {

	@Test
	public void testMatch() {

		TransitionParser p = new TransitionParser();

		assertTrue(p.match("some trash here INFO: TRANSITION: m2 : n1 -> n2"));

		assertEquals("mutator", "m2", p.getMutator());
		assertEquals("source", "n1", p.getSourceState());
		assertEquals("target", "n2", p.getDestState());

	}

}
