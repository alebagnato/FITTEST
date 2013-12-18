/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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
