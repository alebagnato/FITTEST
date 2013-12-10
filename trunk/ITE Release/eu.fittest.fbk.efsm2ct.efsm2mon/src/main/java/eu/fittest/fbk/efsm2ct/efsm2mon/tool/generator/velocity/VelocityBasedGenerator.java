/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.tool.generator.velocity;

//import de.uka.ilkd.pp.Layouter;
//import de.uka.ilkd.pp.NoExceptions;
//import de.uka.ilkd.pp.StringBackend;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Map;
import java.util.Properties;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.runtime.RuntimeConstants;
import org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.FsmTesterException;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.generator.TemplateBasedGeneratorInterface;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Tool;

/**
 * 
 * @author tiella
 */
public class VelocityBasedGenerator implements TemplateBasedGeneratorInterface {

	private static boolean DEBUG_RESOURCE_LOCATION = false;

	@Override
	public String generate(Model model, String packageStr, String templateFileName, Map<String,Object> additionalData) throws FsmTesterException {

		Template template = null;

		Properties props = new Properties();

		// props.setProperty("file.resource.loader.path", "vm");
		props.setProperty(RuntimeConstants.RESOURCE_LOADER, "classpath");
		props.setProperty("classpath.resource.loader.class", ClasspathResourceLoader.class.getName());

		if (DEBUG_RESOURCE_LOCATION) {
			System.out.println("looking for:" + templateFileName);
			Utils.showClasspath();
			System.out.println("template url=" + VelocityBasedGenerator.class.getClassLoader().getResource(templateFileName));
		}

		Velocity.init(props);

		// Returns a Template from the Velocity resource management system
		template = Velocity.getTemplate(templateFileName);

		// creo un contesto e inserisco la map
		VelocityContext context = new VelocityContext();

		Tool toolInfo = new Tool();

		context.put("tool", toolInfo);
		context.put("model", model);
		context.put("packageName", packageStr);
		
		if (additionalData != null) {
			
			for (Map.Entry<String, Object> e : additionalData.entrySet()) {
				
				context.put(e.getKey(), e.getValue());
				
			}
			
		}

		// Object[] keys = context.internalGetKeys();
		//
		// for (int i = 0; i < keys.length; i++) {
		// System.out.println(keys[i] + " = " + context.internalGet((String)
		// keys[i]));
		// }

		Writer writer = new StringWriter();

		if (template != null) {
			try {
				// The AST node structure is merged with the context to produce
				// the final output
				template.merge(context, writer);

				// keys = context.internalGetKeys();
				//
				// for (int i = 0; i < keys.length; i++) {
				// System.out.println(keys[i] + " = " +
				// context.internalGet((String) keys[i]));
				// }

				writer.flush();
				writer.close();
			} catch (IOException ex) {
				throw new FsmTesterException("can't instantiate tamplate", ex);
			}

		} else {
			throw new FsmTesterException("can't instantiate tamplate");
		}

		// StringBackend sb = new StringBackend(80);
		// Layouter<NoExceptions> lo = new Layouter<NoExceptions>(sb, 8);

		// StringBuilder sb = new StringBuilder();

		// Layouter<NoExceptions> stringLayouter =
		// Layouter.getStringLayouter(sb);

		// stringLayouter.print(normOut);

		return writer.toString();

	}

	@Override
	public String generate(Model model, String packageStr, String templateFile) throws FsmTesterException {
		
		return generate(model, packageStr, templateFile, null);
	}
}
