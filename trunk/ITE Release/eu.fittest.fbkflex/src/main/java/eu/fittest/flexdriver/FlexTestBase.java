/*	
 *	License
 *	
 *	This file is part of The SeleniumFlex-API.
 *	
 *	The SeleniumFlex-API is free software: you can redistribute it and/or
 *  modify it  under  the  terms  of  the  GNU  General Public License as 
 *  published  by  the  Free  Software Foundation,  either  version  3 of 
 *  the License, or any later version.
 *
 *  The SeleniumFlex-API is distributed in the hope that it will be useful,
 *  but  WITHOUT  ANY  WARRANTY;  without  even the  implied  warranty  of
 *  MERCHANTABILITY   or   FITNESS   FOR  A  PARTICULAR  PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with The SeleniumFlex-API.
 *	If not, see http://www.gnu.org/licenses/
 *
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
