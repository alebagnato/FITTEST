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

import static eu.fbk.se.utils.Validator.isEmpty;

import java.util.Locale;

import org.stringtemplate.v4.AttributeRenderer;

import eu.fbk.se.utils.Constants;
import eu.fbk.se.webelement.FlashElementBean;

public class FlashElementRender implements AttributeRenderer {


	public String toString(Object obj, String arg1, Locale arg2) {

		if (!(obj instanceof FlashElementBean)){
			return obj.toString();
		}

		StringBuilder builder = new StringBuilder();
		FlashElementBean element = (FlashElementBean)obj;
		
		if (!isEmpty(element.getId()) && !isEmpty(element.getEvent())){
			builder.append("driver.invoke(\""); 
			builder.append(element.getId()); 
			builder.append("\", \"");
			builder.append(element.getEvent()); 
			builder.append("\"");
			
			if (element.getParams().length > 0 && !element.getParams()[0].isEmpty()){
				for (String p : element.getParams()){
					if (!p.contains(Constants.STRING_QUOTE)){
						builder.append(", \"");
						builder.append(p); 
						builder.append("\"");
					} else {
						builder.append(", ");
						builder.append(p);
					}
				}
			}
			
			builder.append(");\n");
			
		} else {
			builder.append("\n// Skip an empty event or id. check the tree and domain input spec\n\n"); 
		}
		
		return builder.toString();
	}

}
