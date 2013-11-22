/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.selenium.renders;

import java.util.Locale;

import org.stringtemplate.v4.AttributeRenderer;

import eu.fbk.se.webelement.WebElementBean;
import static eu.fbk.se.utils.Validator.isEmpty;

public class WebElementRender implements AttributeRenderer {
	private static int elementCounter = 1;

	public String toString(Object o, String formatString, Locale locale) {
		if (!(o instanceof WebElementBean))
			return o.toString();
		
		StringBuilder builder = new StringBuilder();
		
		WebElementBean element = (WebElementBean)o;
		String eName = isEmpty(element.getName()) ? "e" + String.valueOf(elementCounter++) : element.getName();
		if (!isEmpty(element.getXpath())){
			// get element by xpath
			builder.append("WebElement " + eName + " = driver.findElement(By.xpath(\"" + element.getXpath() + "\"));\n");
			builder.append("// WebElement " + eName + " = driver.findElement(By.id(\"" + element.getId() + "\"));\n");
		} else if (!isEmpty(element.getId())){
			// get element by id
			builder.append("WebElement " + eName + " = driver.findElement(By.id(\"" + element.getId() + "\"));\n");
		} else if (!isEmpty(element.getName())){
			builder.append("WebElement " + eName + " = driver.findElement(By.name(\"" + element.getName() + "\"));\n");
		} else {
			builder.append("// TODO specify item id or change to name or xpath \n");
			builder.append("// WebElement " + eName + " = driver.findElement(By.id(\"[id of item]\"));\n");
		}
		builder.append("\n");
 		
 		// fire item event
		if (!isEmpty(element.getXpath()) || !isEmpty(element.getName()) || !isEmpty(element.getId())){
			builder.append("assertNotNull(" + eName + ");\n");
			builder.append(eName + "." + element.getEvent() + ";\n");
		}
 		builder.append("\n");
 		
		return builder.toString();
	}

	@Override
	public String toString() {
		return super.toString();
	}
	
}
