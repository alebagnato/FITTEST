package eu.fbk.se.cte2se.test;

import static org.junit.Assert.*;

import org.junit.Test;

import eu.fbk.se.selenium.renders.FlexElementRender;
import eu.fbk.se.selenium.renders.HttpElementRender;
import eu.fbk.se.transform.AbstractTemplateProvider;
import eu.fbk.se.transform.CTE2Http;
import eu.fbk.se.transform.SeleniumDriverTemplateProvider;
import eu.fbk.se.transform.TransformException;
import eu.fbk.se.webelement.FlexElementBean;
import eu.fbk.se.webelement.HttpElementBean;

public class HttpTransformTest {


	@Test
	public void test0() {

		String cteFile = "src/test/resources/input/utrecht/ctes/p0.cte";
		String domainInputFile = "src/test/resources/input/utrecht/dis.xml";

		extracted("p0",cteFile, domainInputFile);

	}
	
	@Test
	public void test1() {

		String cteFile = "src/test/resources/input/utrecht/ctes/p1.cte";
		String domainInputFile = "src/test/resources/input/utrecht/dis.xml";

		extracted("p1",cteFile, domainInputFile);

	}
	
	@Test
	public void test2() {

		String cteFile = "src/test/resources/input/utrecht/ctes/p2.cte";
		String domainInputFile = "src/test/resources/input/utrecht/dis.xml";

		extracted("p2",cteFile, domainInputFile);

	}

	private void extracted(String packageName, String cteFile, String domainInputFile) {
		String templateGroupFile = "src/main/resources/templates/junit.httpunit.stg";
		AbstractTemplateProvider templateProvider = new SeleniumDriverTemplateProvider(
				templateGroupFile);

		templateProvider.registerRenderer(HttpElementBean.class,
				new HttpElementRender("http://www.modellio.com"));

		if (templateProvider.isTemplateReady()) {

			// ready

			
			String selenDriver = "XXXDriver";
			String className = "HttpTest";
			String targetPage = "XXXTargetPage";

			try {
				CTE2Http transformer = new CTE2Http(templateProvider,
						packageName, className, targetPage, selenDriver);

				String outputFolder = "target/output/php";
				boolean validTestOnly = false;

				transformer.transform(cteFile, domainInputFile, outputFolder, validTestOnly);

				// transformer.transform(cteFile, domainInputFile, outputFolder,
				// validTestOnly);

			} catch (TransformException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();

				fail("can't perform transformation:" + e);

			}

		}
	}

}
