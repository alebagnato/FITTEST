package eu.fbk.se.selenium.renders;

import java.util.Locale;

import org.stringtemplate.v4.AttributeRenderer;
import eu.fbk.se.webelement.HttpElementBean;

/**
 * Renders an element of class HttpElementBean for the sake of generating a
 * HttpUnit-based test case
 * 
 * @author tiella
 * 
 */

public class HttpElementRender implements AttributeRenderer {

	public final static String WC_INSTANCE_ID = "wc"; // name of the instance of
														// web conversation used
														// in the template
	public static final String GET_REQUEST_INSTANCE_ID = "getReq"; // name of
																	// the
																	// instance
																	// of get
																	// request
																	// used in
																	// the
																	// template
	public static final String POST_REQUEST_INSTANCE_ID = "postReq"; // name of
																		// the
																		// instance
																		// of
																		// post
																		// request
																		// used
																		// in
																		// the
																		// template
	public static final String RESPONSE_INSTANCE_ID = "res"; // name of the
																// instance of
																// response used
																// in the
																// template

	private String urlPrefix;

	private boolean debug = false;

	public HttpElementRender(String urlPrefix) {
		this.urlPrefix = urlPrefix;
	}

	@Override
	public String toString(Object o, String formatString, Locale locale) {

		if (!(o instanceof HttpElementBean))
			return o.toString();

		StringBuilder builder = new StringBuilder();

		HttpElementBean element = (HttpElementBean) o;

		try {
			
			builder.append(String.format("// event: %s\n\n",element.getSequenceName()));

			if (element.getEvent().equals(HttpElementBean.GET_METHOD)) {

				renderGet(builder, element);

			} else if (element.getEvent().equals(HttpElementBean.POST_METHOD)) {

				renderPost(builder, element);

			} else {

				renderUnformatted(builder, element);

			}
			
			builder.append("\n");

		} catch (Exception t) {

			builder.append("\n\n****************************************************\n");
			builder.append("\n\n****************************************************\n");
			builder.append("\n\n****************************************************\n");
			builder.append("\n\n****************************************************\n");
			builder.append("!!! Generation failed: \n");
			builder.append(t.toString() + "\n");
			builder.append("\n\n****************************************************\n");
			builder.append("\n\n****************************************************\n");
			builder.append("\n\n****************************************************\n");
			builder.append("\n\n****************************************************\n");

		}

		return builder.toString();
	}

	private void renderGet(StringBuilder builder, HttpElementBean elem) {

		String url = urlPrefix + elem.getUrl();

		builder.append(GET_REQUEST_INSTANCE_ID);
		builder.append(" = new GetMethodWebRequest(");
		builder.append('"');
		builder.append(url);
		builder.append('"');
		builder.append(");\n");

		builder.append(RESPONSE_INSTANCE_ID);
		builder.append("= wc.getResponse(");
		builder.append(GET_REQUEST_INSTANCE_ID);
		builder.append(");\n");

		builder.append("//Test\n");
		builder.append(String.format(
				"if (!res.getURL().toString().equals(\"%s\")) {\n", url));
		builder.append(String.format("fail(\"Wrong request: %s\");\n",url));
		builder.append("}\n");

		// renderUnformatted(builder, elem);

	}

	private void renderPost(StringBuilder builder, HttpElementBean elem) {

		
		
		builder.append(POST_REQUEST_INSTANCE_ID);
		builder.append(" = new PostMethodWebRequest(");
		builder.append('"');
		builder.append(urlPrefix);
		builder.append(elem.getUrl());
		builder.append('"');
		builder.append(");\n");

		for (String p : elem.getParams()) {

			if (debug) {
				System.out.println("parameter=" + p);
			}

			String[] pParts = p.split("=");

			builder.append(POST_REQUEST_INSTANCE_ID);
			builder.append(".setParameter(");
			builder.append('"');
			builder.append(pParts[0]);
			builder.append('"');
			builder.append(",");
			builder.append('"');
			builder.append(pParts[1]);
			builder.append('"');
			builder.append(");\n");

		}

		builder.append(RESPONSE_INSTANCE_ID);
		builder.append("= wc.getResponse(");
		builder.append(POST_REQUEST_INSTANCE_ID);
		builder.append(");\n");

		// renderUnformatted(builder, elem);

	}

	/**
	 * used for debugging purpose
	 * 
	 * @param builder
	 * @param element
	 */

	private void renderUnformatted(StringBuilder builder,
			HttpElementBean element) {

		builder.append("// UNFORMATTED: ");
		builder.append(element.getEvent()).append(", [");
		// builder.append(element.getId()).append(", ");

		for (String p : element.getParams()) {
			builder.append(p).append(" ");
		}

		builder.append("]");

		builder.append(element.getUrl()).append(", ");

		builder.replace(builder.length() - 2, builder.length(), "\n");
	}

}
