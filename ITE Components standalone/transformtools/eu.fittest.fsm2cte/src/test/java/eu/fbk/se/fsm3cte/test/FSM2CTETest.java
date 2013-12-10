/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.fsm3cte.test;

import java.io.File;

import eu.fbk.se.fsm.transformer.FSM2CTE;
import junit.framework.TestCase;

/**
 * The class <code>FSM2CTETest</code> contains tests for the class {@link
 * <code>FSM2CTE</code>}
 *
 * @pattern JUnit Test Case
 *
 * @generatedBy CodePro at 4/13/11 1:51 PM
 *
 * @author cunduy
 *
 * @version $Revision$
 */
public class FSM2CTETest extends TestCase {

	private String projectHome = System.getProperty("user.dir");
	
	/**
	 * Construct new test instance
	 *
	 * @param name the test name
	 */
	public FSM2CTETest(String name) {
		super(name);
	}

	/**
	 * Perform pre-test initialization
	 *
	 * @throws Exception
	 *
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		// Add additional set up code here
	}

	/**
	 * Run the void transform(String, String, String) method test
	 */
	public void testTransformWithDomainInputs() {
//		
//		FSM2CTE fixture = new FSM2CTE();
////		String domainInput = projectHome + File.separatorChar + "data" + File.separatorChar + "xinput.xml";
//		String domainInput = "/Users/cdnguyen/workspace/ITEexample/Models/manual/CuteNews.xml";// new File(projectHome, "src/test/data/xinput2.xml").getAbsolutePath();
//		String inputModel =  "/Users/cdnguyen/workspace/ITEexample/Models/sebastian/client/concrete/concrete.fsm"; //new File(projectHome,"src/test/data/cart1.fsm").getAbsolutePath();
//		
//		String outputFolder = new File(projectHome, "target/test/output").getAbsolutePath();
//		// TODO clean output folder
//		
//		fixture.transform(domainInput, inputModel, outputFolder, true, null);
//
//		// check if there's something in the output folder
//		File f = new File(outputFolder);
//		assertTrue(f.list().length > 0);
	}

	/**
	 * Run the void transform(String, String, String) method test
	 */
	public void testTransform() {
		
//		FSM2CTE fixture = new FSM2CTE();
//		String inputModel =  new File(projectHome,"src/test/data/cart4.fsm").getAbsolutePath();
//		String outputFolder = new File(projectHome, "target/test/output2").getAbsolutePath();
//		// TODO clean output folder
//		
//		fixture.transform(inputModel, outputFolder);
//		
//		// check if there's something in the output folder
//		File f = new File(outputFolder);
//		assertTrue(f.list().length > 0);
	}
}

