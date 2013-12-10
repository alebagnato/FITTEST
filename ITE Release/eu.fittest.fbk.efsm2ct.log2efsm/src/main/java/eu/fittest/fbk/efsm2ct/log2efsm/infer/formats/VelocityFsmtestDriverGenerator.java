package eu.fittest.fbk.efsm2ct.log2efsm.infer.formats;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Properties;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.runtime.RuntimeConstants;
import org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader;

import eu.fittest.fbk.efsm2ct.log2efsm.infer.GeneratorException;

/**
 * Simple println-based code generator
 * 
 * @author tiella
 * 
 */

public class VelocityFsmtestDriverGenerator {

	private MutatorDescriptionSet mds;
	private String className;
	private String packageName;
	private InspectorDescriptionSet ids;

	public VelocityFsmtestDriverGenerator(String packageName, String className, MutatorDescriptionSet mds, InspectorDescriptionSet ids) throws FileNotFoundException {
		this.mds = mds;
		this.ids = ids;
		this.className = className;
		this.packageName = packageName;
	}

	public void generate(String outputDirectoryFn) throws GeneratorException {

		File outDir = new File(outputDirectoryFn);
		File outFile = new File(outDir, className + ".java");

		Properties props = new Properties();

		props.setProperty(RuntimeConstants.RESOURCE_LOADER, "classpath");
		props.setProperty("classpath.resource.loader.class", ClasspathResourceLoader.class.getName());

		Velocity.init(props);

		// Returns a Template from the Velocity resource management system
		Template template = Velocity.getTemplate("vm/SutDriver.vm");

		if (template != null) {

			VelocityContext context = new VelocityContext();
			context.put("className", className);

			context.put("inspectors", ids);
			context.put("mutators", mds);

			Writer writer;

			try {
				writer = new FileWriter(outFile);
			} catch (IOException e) {
				throw new GeneratorException("can't instantiate template", e);
			}

			try {
				// The AST node structure is merged with the context to produce
				// the final output
				template.merge(context, writer);
				writer.flush();
				writer.close();
			} catch (IOException ex) {
				throw new GeneratorException("can't instantiate template", ex);
			}

		} else {
			throw new GeneratorException("can't instantiate template");
		}

	}

}
