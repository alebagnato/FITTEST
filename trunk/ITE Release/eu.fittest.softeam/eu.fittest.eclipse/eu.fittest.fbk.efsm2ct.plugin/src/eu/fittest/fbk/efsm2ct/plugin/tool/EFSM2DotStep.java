package eu.fittest.fbk.efsm2ct.plugin.tool;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintWriter;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.FsmTesterException;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.Generator;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.parser.FsmParser;

public class EFSM2DotStep implements ToolStep {
	
	private String fsmFilePath;
	private String destDirName;


	public String getFsmFilePath() {
		return fsmFilePath;
	}

	public void setFsmFilePath(String fsmFilePath) {
		this.fsmFilePath = fsmFilePath;
	}

	public String getDestDirName() {
		return destDirName;
	}

	public void setDestDirName(String destDirName) {
		this.destDirName = destDirName;
	}



	@Override
	public void execute() throws ToolStepException {

		try {
		
		Model m = FsmParser.parse(new FileReader(fsmFilePath));

		StringBuilder annotation = new StringBuilder();
		
		if (!m.checkSemantic(annotation)) {
			throw new FsmTesterException("semantic errors in input:" + annotation);
		}

		
		String modelsDirName = new File(fsmFilePath).getParent();

		generateDot(m, modelsDirName);

			
		} catch (Exception ex) {
			throw new ToolStepException("can't execute:",ex);
		}

	}
	


	private void generateDot(Model m, String destDirName) throws FileNotFoundException, FsmTesterException {

		boolean onlyOut = false;
		
		File destFile = new File(destDirName, m.getSutName()+".dot");
		PrintWriter pw = new PrintWriter(destFile);
		
		Generator generator = new Generator();
		
		generator.setPackageName("");
		generator.setAdditionalData(null);
		generator.setFout(pw);
		generator.setModel(m);
		generator.setTemplateFileName("eu/fittest/fbk/efsm2ct/plugin/vm/dot-fittest-simpl.vm");
		
		generator.run();
		
		// Main.runDot(m, packageName, destDirName, onlyOut, OutputType.DOT_FITTEST);

	}


}
