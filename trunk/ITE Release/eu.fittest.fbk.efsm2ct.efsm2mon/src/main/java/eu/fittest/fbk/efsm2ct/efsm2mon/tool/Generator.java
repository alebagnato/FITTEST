package eu.fittest.fbk.efsm2ct.efsm2mon.tool;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Map;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.generator.TemplateBasedGeneratorInterface;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.generator.velocity.VelocityBasedGenerator;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.indent.Indenter;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;

public class Generator {

	private Model model;
	private String packageName;
	private Writer fout;
	private String templateFileName;
	private Indenter indenter;
	private Map<String, Object> additionalData;
	
	

	public void setModel(Model model) {
		this.model = model;
	}



	public void setPackageName(String packageName) {
		this.packageName = packageName;
	}



	public void setFout(Writer fout) {
		this.fout = fout;
	}



	public void setTemplateFileName(String templateFileName) {
		this.templateFileName = templateFileName;
	}



	public void setIndenter(Indenter indenter) {
		this.indenter = indenter;
	}


	public void setAdditionalData(Map<String, Object> additionalData) {
		this.additionalData = additionalData;
	}



	public void run() throws FsmTesterException {

		TemplateBasedGeneratorInterface gen = new VelocityBasedGenerator();

		String sourceCode = gen.generate(model, packageName, templateFileName, additionalData);

		if (indenter != null) {
			sourceCode = indenter.indent(sourceCode);
		}

		PrintWriter pw = new PrintWriter(fout);

		pw.println(sourceCode);

		pw.close();

	}

}
