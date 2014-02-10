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
						
						if (p.equals("@EMPTY@")) {
							p = "";
						}
						
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
