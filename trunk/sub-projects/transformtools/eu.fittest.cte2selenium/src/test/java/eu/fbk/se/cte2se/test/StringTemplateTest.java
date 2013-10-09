/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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
