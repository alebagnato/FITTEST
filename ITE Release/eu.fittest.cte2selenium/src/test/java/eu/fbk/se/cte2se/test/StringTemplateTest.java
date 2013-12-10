package eu.fbk.se.cte2se.test;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

import eu.fbk.se.selenium.renders.WebElementRender;
import eu.fbk.se.webelement.WebElementBean;

public class StringTemplateTest {

	/**
	 * @param args
	 */
	
	@Test
	public void testST(){
		String templateGroupFile = new File(System.getProperty("user.dir"), "src/main/resources/templates/junit.wdriver.stg").getAbsolutePath();
		
		STGroup stgGroup = new STGroupFile(templateGroupFile);
//		stgGroup.load();
		
		stgGroup.registerRenderer(WebElementBean.class, new WebElementRender());
		
		List<WebElementBean> elements = new ArrayList<WebElementBean>();
		elements.add(new WebElementBean("e1", "//btnAdd", "click()"));
		elements.add(new WebElementBean("e2", "//button[@id='btnAdd' and @onclick=\"addToCart('sou001')\"]", "click()"));
		
		ST tc = stgGroup.getInstanceOf("TestCase");
		tc.add("TestDescription", "Hello World");
		tc.add("TestName", "test1");
		tc.add("PageName", "http://localhost:8080/cart");
		tc.add("WElements", elements);
		
		
		ST tc2 = stgGroup.getInstanceOf("TestCase");
		tc2.add("TestDescription", "Test Case 2");
		tc2.add("TestName", "test2");
		tc2.add("PageName", "http://localhost:8080/cart");
		tc2.add("WElements", null);


		ST header = stgGroup.getInstanceOf("TestHeader");
		header.add("PackageName", "test.pck");
		header.add("ClassName", "CartTest");
		header.add("WebDriver", "FirefoxDriver");

		ST footer = stgGroup.getInstanceOf("TestFooter");
		footer.add("ClassName", "CartTest");
		footer.add("WebDriver", "FirefoxDriver");
		
		System.out.println(header.render());
		System.out.println(tc.render());
		System.out.println(tc2.render());
		System.out.println(footer.render());		
	}
	

}
