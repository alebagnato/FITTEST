/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/

/* Contributed by Black Pepper Software Ltd.  */

/*
 * Modified for FITTEST project by CuND/FBK
 * Date: May 2011
 * 
 */

package eu.fittest.flexdriver;

import java.io.FileInputStream;
import java.util.Properties;

import net.sourceforge.seleniumflexapi.FlexSelenium;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import com.thoughtworks.selenium.DefaultSelenium;
import com.thoughtworks.selenium.Selenium;

public abstract class FlexTestBase {

	static Properties properties;
	static Selenium selenium;
	private FlexSelenium flexSelenium;

	/**
	 * Perform necessary initialisation prior to any test being executed.
	 * 
	 * @throws Exception
	 *             if initialisation fails
	 */
	@BeforeClass
	public static void setUpClass() throws Exception {
		properties = new Properties();
		properties.load(new FileInputStream("test.properties"));

		selenium = new DefaultSelenium(
				properties.getProperty("selenium.serverHost"),
				Integer.valueOf(properties.getProperty("selenium.serverPort")),
				properties.getProperty("selenium.browserStartCommand"),
				properties.getProperty("selenium.browserUrl"));
		// selenium.setExtensionJs(readFileAsString("lib/user-extensions.js"));
		getContext().start();
	}

	// private static String readFileAsString(String filePath) throws
	// java.io.IOException{
	// byte[] buffer = new byte[(int) new File(filePath).length()];
	// BufferedInputStream f = null;
	// try {
	// f = new BufferedInputStream(new FileInputStream(filePath));
	// f.read(buffer);
	// } finally {
	// if (f != null) try { f.close(); } catch (IOException ignored) { }
	// }
	// return new String(buffer);
	// }

	@Before
	public void setUp() throws Exception {
		flexSelenium = new FlexSelenium(getContext(),
				properties.getProperty("application.name"));
		getContext().open(properties.getProperty("selenium.browserPage"));

		String speed = properties.getProperty("selenium.speed");
		getContext().setSpeed(speed);
	}

	/**
	 * Tidy up resources after every test has been executed.
	 * 
	 * @throws Exception
	 *             if the tidy up fails
	 */
	@AfterClass
	public static void tearDownClass() throws Exception {
		getContext().stop();
	}

	/**
	 * Retrieve the context for accessing Selenium.
	 * 
	 * @return the Selenium context
	 */
	protected static Selenium getContext() {
		return selenium;
	}

	protected FlexSelenium getFlashApp() {
		return flexSelenium;
	}
}
