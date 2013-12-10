package eu.fittest.fbk.efsm2ct.tools.rewrite;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

public class TestFixerTest {

	@Test
	public void testFixit() throws IOException {
		
		TestFixer tf = new TestFixer();
		
		tf.readInput(new File("src/test/tofix/TestTestFlexstore.java"));
		
		tf.fixit();
		
		System.out.println(tf.getInput());
		
	}

}
