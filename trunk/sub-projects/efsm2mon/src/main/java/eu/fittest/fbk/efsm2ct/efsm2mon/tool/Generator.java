/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.fbk.efsm2ct.efsm2mon.tool;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.generator.TemplateBasedGeneratorInterface;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.generator.velocity.VelocityBasedGenerator;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.indent.Indenter;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Map;

/**
 * 
 * @author tiella
 */
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
