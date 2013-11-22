/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.android.renders;

import static eu.fbk.se.utils.Validator.isEmpty;

import java.util.Locale;

import org.stringtemplate.v4.AttributeRenderer;

import eu.fbk.se.android.element.AndroidElementBean;
import eu.fbk.se.utils.Constants;

public class AndroidElementRender implements AttributeRenderer {

	public String toString(Object obj, String arg1, Locale arg2) {
		if (!(obj instanceof AndroidElementBean)) {
			return obj.toString();
		}

		StringBuilder builder = new StringBuilder();
		AndroidElementBean element = (AndroidElementBean) obj;

		if (!isEmpty(element.getId()) && !isEmpty(element.getEvent())) {

			if (element.getEvent().endsWith(")")) {
				// special param has been specified
				builder.append("driver.");
				builder.append(element.getEvent());
				builder.append(";\n");

			} else {

				builder.append("driver.");
				builder.append(element.getEvent());
				builder.append("(");
				
				if (!"#".equals(element.getId())){
					builder.append(element.getId());
					if (element.getParams().length > 0
							&& !isEmpty(element.getParams()[0])){
						builder.append(", ");
					}
				}

				if (element.getParams().length > 0
						&& !element.getParams()[0].isEmpty()) {
					int i = 1;
					for (String p : element.getParams()) {
						builder.append(p);
						
						if (i < element.getParams().length)
							builder.append(", ");
						i++;
					}
				}
				builder.append(");\n");
			}


		} else {
			builder.append("\n// check this element: " + element.getId() + ", "
					+ element.getEvent() + "\n\n");
		}

		return builder.toString();
	}

}
