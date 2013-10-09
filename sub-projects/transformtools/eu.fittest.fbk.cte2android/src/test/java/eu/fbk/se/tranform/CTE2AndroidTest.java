/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.tranform;

import java.io.File;
import java.io.FilenameFilter;
import java.util.List;

import org.junit.Assert;
import org.junit.Ignore;

import eu.fbk.se.transform.AbstractTemplateProvider;
import eu.fbk.se.transform.CTE2FlexSelenium;
import eu.fbk.se.transform.CTE2RobotiumAndroid;
import eu.fbk.se.transform.SeleniumDriverTemplateProvider;
import eu.fbk.se.transform.TransformException;

public class CTE2AndroidTest {
	private String projectHome = System.getProperty("user.dir");
	private String sep = File.separator;
	String eclipseRoot = "/home/cunduy/workspace-mobile";

	
	/**
	 * Run the transformTest test
	 * 
	 * @throws TransformException
	 */

	@Ignore
	public void testTransformTest() throws TransformException {
		String templateGroupFile = projectHome
				+ "/src/main/resources/templates/robotium.driver.stg";

		AbstractTemplateProvider templateProvider = new SeleniumDriverTemplateProvider(
				templateGroupFile);
		
		CTE2RobotiumAndroid fixture = new CTE2RobotiumAndroid(templateProvider,
				"com.example.android.contactmanager.test.generated", "P0",
				"com.example.android.contactmanager", "ContactManager");


		String cteFile = eclipseRoot + sep + "ContactManagerTest" + sep
				+ "model" + sep + "ctes" + sep + "p0.cte";

		String outputFolder = eclipseRoot + sep + "ContactManagerTest" + sep
				+ "src";

		String xinputFile = eclipseRoot + sep + "ContactManagerTest" + sep
				+ "model" + sep + "ContactManager.xml";

		boolean result = fixture.transform(cteFile, xinputFile, outputFolder,
				false);

			
		// add additional test code here
		Assert.assertEquals(true, result);
		
	}
	
	
	
	/**
	 * 
	 * @throws TransformException
	 */
	@Ignore
	public void testTransformFolder() throws TransformException{
		String templateGroupFile = projectHome
				+ "/src/main/resources/templates/robotium.driver.stg";

		AbstractTemplateProvider templateProvider = new SeleniumDriverTemplateProvider(
				templateGroupFile);
		
		String outputFolder = eclipseRoot + sep + "ContactManagerTest" + sep
				+ "src";
		
		String xinputFile = eclipseRoot + sep + "ContactManagerTest" + sep
				+ "model" + sep + "ContactManager.xml";

		String cteFolder = eclipseRoot + sep + "ContactManagerTest" + sep
				+ "model" + sep 
				+ "ctes";
		
		File f = new File(cteFolder);
		
		String[] fileList = f.list(new FilenameFilter() {
			public boolean accept(File dir, String name) { 
				return name.endsWith(".cte");  
			}
		});
		
		for (String s : fileList){
		
			String className = s.replace(".cte","").toUpperCase() + "Test";
			
			CTE2RobotiumAndroid fixture = new CTE2RobotiumAndroid(templateProvider,
					"com.example.android.contactmanager.test.generated", className,
					"com.example.android.contactmanager", "ContactManager");
		
		
			String cteFile = eclipseRoot + sep + "ContactManagerTest" + sep
					+ "model" + sep 
					+ "ctes" + sep 
					+ s;

			boolean result = fixture.transform(cteFile, xinputFile, outputFolder,
					false);

			Assert.assertTrue(result);
		}


	}
	
}
