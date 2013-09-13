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
