package eu.fbk.se.selenium.renders;

import java.util.Locale;

import org.stringtemplate.v4.AttributeRenderer;

import eu.fbk.se.webelement.FlexElementBean;
import static eu.fbk.se.utils.Validator.isEmpty;

public class FlexElementRender implements AttributeRenderer {

	public String toString(Object o, String arg1, Locale arg2) {
		if (!(o instanceof FlexElementBean))
			return o.toString();

		StringBuilder builder = new StringBuilder();
		FlexElementBean element = (FlexElementBean) o;

		if (!isEmpty(element.getId()) && !isEmpty(element.getEvent())) {
			if ("clickButton".equals(element.getEvent())) {
				builder.append("driver.clickButton(\"" + element.getId()
						+ "\");\n");
			} else {
				if (!element.getParams().isEmpty()){
					builder.append("driver." + element.getEvent() + "(\""
							+ element.getId() + "\", " + element.getParams()
							+ ");\n");
				} else {
					builder.append("driver." + element.getEvent() + "(\""
							+ element.getId() + "\");\n");
				}
			}

		} else {
			builder.append("// TODO specify item id or change to name or xpath \n");
			builder.append("// Event: " + element.getEvent() + ", element = "
					+ element.getId() + "\n");
		}
		builder.append("\n");

		return builder.toString();
	}

}
