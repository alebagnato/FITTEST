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