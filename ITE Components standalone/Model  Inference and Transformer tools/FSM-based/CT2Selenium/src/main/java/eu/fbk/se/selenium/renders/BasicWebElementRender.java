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

import eu.fbk.se.fsm.xinput.WebElementType;
import eu.fbk.se.utils.Constants;
import eu.fbk.se.webelement.WebElementBean;
import static eu.fbk.se.utils.Validator.isEmpty;

/**
 * Render for basic web elements, input, button, select, (check) 
 * @author cunduy
 *
 */
public class BasicWebElementRender implements AttributeRenderer {
	private static int elementCounter = 1;

	public String toString(Object o, String formatString, Locale locale) {
		if (!(o instanceof WebElementBean))
			return o.toString();
		
		StringBuilder builder = new StringBuilder();
		
		WebElementBean element = (WebElementBean)o;
		String eName = isEmpty(element.getName()) ? "e" : standardizeIdentifierName(element.getName());
		
		eName = eName + String.valueOf(elementCounter++);
		
		boolean elementFound = false;
		if (!isEmpty(element.getId())){
			// get element by id
			builder.append("WebElement " + eName + " = driver.findElement(By.id(\"" + element.getId() + "\"));\n");
			elementFound = true;
		} else if (!isEmpty(element.getXpath())){
			// get element by xpath
			builder.append("WebElement " + eName + " = driver.findElement(By.xpath(\"" + element.getXpath() + "\"));\n");
			elementFound = true;
		} else if (!isEmpty(element.getName())){
			builder.append("WebElement " + eName + " = driver.findElement(By.name(\"" + element.getName() + "\"));\n");
			elementFound = true;
		} else if (!isEmpty(element.getLinkText())){
			builder.append("WebElement " + eName + " = driver.findElement(By.linkText(\"" + element.getLinkText() + "\"));\n");
			elementFound = true;
		} else if (!isEmpty(element.getCss())){
			builder.append("WebElement " + eName + " = driver.findElement(By.cssSelector(\"" + generateStringCode(element.getCss()) + "\"));\n");
			elementFound = true;
		} else if (!isEmpty(element.getPartialLinkText())){
			builder.append("WebElement " + eName + " = driver.findElement(By.partialLinkText(\"" + element.getPartialLinkText() + "\"));\n");
			elementFound = true;
		} else if (!isEmpty(element.getTagName())){
			builder.append("WebElement " + eName + " = driver.findElement(By.tagName(\"" + element.getTagName() + "\"));\n");
			elementFound = true;
//		} else {
//			builder.append("// TODO specify item id or change to name or xpath \n");
//			builder.append("// WebElement " + eName + " = driver.findElement(By.id(\"[id of item]\"));\n");
		}
		builder.append("\n");
 		
 		// fire item event
		if (elementFound){
			builder.append("assertNotNull(" + eName + ");\n");
			if (element.getType().equals(WebElementType.INPUT)){
				
				if ("type".equals(element.getEvent())){
					String tmp = element.getParams();
					if (tmp.indexOf(Constants.STRING_QUOTE) == -1){
						tmp = Constants.STRING_QUOTE + tmp + Constants.STRING_QUOTE;
					}
					builder.append(eName + ".sendKeys(" + tmp + ");\n");
					
					// hack for cyclos
					// builder.append("clearAndType(" + eName + ", " + tmp + ");\n");
				}  
			} else if (element.getType().equals(WebElementType.SELECT)){
				builder.append("Select s"+ eName +" = new Select(" + eName + ");\n");
				if (element.getParams() == null || element.getParams().length() == 0){
					builder.append("s"+ eName +".selectByValue(\"\");\n");
				} else {
					builder.append("s"+ eName +".selectByVisibleText(" + element.getParams() + ");\n");
				}
			} else {
				if (element.getEvent().indexOf(")") == -1)
					builder.append(eName + "." + element.getEvent() + "();\n");
				else 
					builder.append(eName + "." + element.getEvent() + ";\n");
			}
		} else if (!isEmpty(element.getUrl())) {
			if ("open".equals(element.getEvent())){
				builder.append("driver.get(baseUrl + \"" + element.getUrl() + "\");\n");
			} else {
				builder.append("driver." + element.getEvent() + "(\"" + element.getUrl() + "\");\n");
			}
		} else {
			builder.append("driver." + element.getEvent() + ";\n");
		}
		
		if ("wait".equals(element.getEvent())){
//			builder.append("TestUtils.waitForTimeout(driver, 5);\n");
			builder.append("waitForTimeout(driver, 5);\n");
		}
		
 		builder.append("\n");
 		
		return builder.toString();
	}
	
	private String generateStringCode(String input){
		return input.replaceAll("\"", "\\\\\"");
	}
	
	private String standardizeIdentifierName(String input){
		if (input == null || input.length() == 0){
			return "null";
		}
		
		String output = "";
		
		for (int i = 0; i< input.length(); i++){
			if (isAlphabetic(input.charAt(i))){
				output = output + input.charAt(i);
			}
		}
		return output;
	}
	
	private boolean isAlphabetic(int codePoint) {
        return (((((1 << Character.UPPERCASE_LETTER) |
            (1 << Character.LOWERCASE_LETTER) |
            (1 << Character.TITLECASE_LETTER) |
            (1 << Character.MODIFIER_LETTER) |
            (1 << Character.OTHER_LETTER) |
            (1 << Character.LETTER_NUMBER)) >> Character.getType(codePoint)) & 1) != 0);
        
        // ||    CharacterData.of(codePoint).isOtherAlphabetic(codePoint)
    }

	@Override
	public String toString() {
		return super.toString();
	}
	
}
