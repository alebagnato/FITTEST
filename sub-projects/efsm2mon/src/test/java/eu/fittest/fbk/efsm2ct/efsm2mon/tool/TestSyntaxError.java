package eu.fittest.fbk.efsm2ct.efsm2mon.tool;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.parser.FsmParser;

import java.io.FileNotFoundException;
import java.io.FileReader;
import static org.junit.Assert.*;
import org.junit.Test;

public class TestSyntaxError {

	@Test
	public void testRunJavaSynErr() throws FileNotFoundException {

		try {
			String fsmFilePath = "src/test/input/efsms/Cart_Syntax_Error.efsm";
			String packageName = "cart";
			Model m = FsmParser.parse(new FileReader(fsmFilePath));
	
			StringBuilder annotation = new StringBuilder();

			if (!m.checkSemantic(annotation)) {
				fail("semantic errors in input:" + annotation);
			}
			
			fail("an exception was expected here");
			
		} catch (FsmTesterException ex) {
			System.out.println("DON'T WORRY about NoViableAltException, it is expected");
		}

	}

}
