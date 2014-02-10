package eu.fbk.se.cte2se.test;

import static org.junit.Assert.*;

import org.junit.Test;

import eu.fbk.se.selenium.renders.FlexElementRender;
import eu.fbk.se.selenium.renders.HttpElementRender;
import eu.fbk.se.transform.AbstractTemplateProvider;
import eu.fbk.se.transform.CTE2FlashSelenium;
import eu.fbk.se.transform.CTE2Http;
import eu.fbk.se.transform.SeleniumDriverTemplateProvider;
import eu.fbk.se.transform.TransformException;
import eu.fbk.se.webelement.FlexElementBean;
import eu.fbk.se.webelement.HttpElementBean;

public class FlashTransformTest {

	@Test
	public void test1() {
		
		String cteFile = "src/test/resources/input/flexstore/cte/p7.cte";	
		String domainInputFile = "src/test/resources/input/flexstore/xinput.xml";

		extracted(cteFile, domainInputFile);
		
	}
	
	
	private void extracted(String cteFile, String domainInputFile) {
		
		String templateGroupFile = "src/main/resources/templates/junit.flashdriver.stg";
		AbstractTemplateProvider templateProvider = new SeleniumDriverTemplateProvider(templateGroupFile );
		
		// templateProvider.registerRenderer(HttpElementBean.class, new HttpElementRender("http://www.modellio.com"));
		
		if (templateProvider.isTemplateReady()){
			
			// ready
			
			String packageName = "testpackage";
			String selenDriver = "XXXDriver";
			String className = "FlashTest";
			String targetPage = "XXXTargetPage";
			
			try {
				CTE2FlashSelenium transformer = new CTE2FlashSelenium(templateProvider, packageName, 
						className, targetPage,  selenDriver);
				
							
				String outputFolder = "target/output/flash";
				boolean validTestOnly = false;

						
				transformer.transform(cteFile, domainInputFile, outputFolder, validTestOnly);
			
				// transformer.transform(cteFile, domainInputFile, outputFolder, validTestOnly);

				
				
			} catch (TransformException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				
				fail("can't perform transformation:"+e);
				
			}
			
		}
	}

}
