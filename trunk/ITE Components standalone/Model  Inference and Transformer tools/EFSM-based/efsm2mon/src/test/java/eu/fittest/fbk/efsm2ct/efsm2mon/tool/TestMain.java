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

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.Main.OutputType;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.parser.FsmParser;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;

public class TestMain {

	private static String fsmFilePath = "src/test/input/efsms/Cart.efsm";
	private static String packageName = "cart";
	private Model m;

	@Before
	public void initModel() throws FileNotFoundException, FsmTesterException {

		m = parseModel(fsmFilePath);

	}

	private Model parseModel(String file) throws FileNotFoundException,
			FsmTesterException {

		Model m = FsmParser.parse(new FileReader(file));

		StringBuilder annotation = new StringBuilder();

		if (!m.checkSemantic(annotation)) {
			fail("semantic errors in input:" + annotation);
		}
		return m;
	}

	// @Test
	public void testMain() {
		Main.usage();
	}

	// @Test
	public void testRunJava() throws FsmTesterException, IOException {

		String destDirName = "target/output/gensrc";
		boolean onlyOut = false;
		boolean hasRunControl = true;
		String tmplFileName = "eu/fittest/fbk/efsm2ct/efsm2mon/vm/fsm-int.vm";

		Main.runJava(m, packageName, "Test", destDirName, onlyOut,
				hasRunControl, tmplFileName);

	}

	@Test
	public void testRunJava_5898() throws FsmTesterException, IOException {

		String destDirName = "target/output/gensrc";
		boolean onlyOut = false;
		boolean hasRunControl = true;
		String tmplFileName = "eu/fittest/fbk/efsm2ct/efsm2mon/vm/fsm-int.vm";

		
		Model m1 = parseModel("src/test/input/efsms/flexstore_5898.efsm");
		
		Main.runJava(m1, packageName, "Test", destDirName, onlyOut,
				hasRunControl, tmplFileName);

	}

	// @Test
	public void testRunDot() throws FsmTesterException, IOException {

		String destDirName = "target/output/dot";
		boolean onlyOut = false;
		OutputType otype = OutputType.DOT_FITTEST;

		File d = new File(destDirName);
		d.mkdirs();

		Model m = parseModel("src/test/input/efsms/flexstore_v3.efsm");

		Main.runDot(m, packageName, destDirName, onlyOut, otype);
	}

	// @Test
	public void testRunGml() throws FsmTesterException, IOException {

		String destDirName = "target/output/gml";
		boolean onlyOut = false;
		OutputType otype = OutputType.GML_ALIAS;

		Main.runGml(m, packageName, destDirName, onlyOut, otype);
	}

}
