package eu.fittest.fbk.efsm2ct.plugin.tool;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.FsmTesterException;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.Generator;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.parser.FsmParser;

public class EFSM2MonitorStep implements ToolStep {

	private boolean generateMockup = true;

	private String fsmFilePath;
	private String destDirName;
	private String packagePrefix;

	public boolean isGenerateMockup() {
		return generateMockup;
	}

	public boolean isGenerateActualDriver() {
		return !generateMockup;
	}

	public void setGenerateActualDriver(boolean value) {
		this.generateMockup = !value;
	}

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

	public String getPackagePrefix() {
		return packagePrefix;
	}

	public void setPackagePrefix(String packagePrefix) {
		this.packagePrefix = packagePrefix;
	}

	@Override
	public void execute() throws ToolStepException {

		try {

			Model m = FsmParser.parse(new FileReader(fsmFilePath));

			StringBuilder annotation = new StringBuilder();

			if (!m.checkSemantic(annotation)) {
				throw new FsmTesterException("semantic errors in input:"
						+ annotation);
			}

			generateJavaMonitor(m);

			if (generateMockup) {
				generateJavaMockupDriver(m);
			} else {
				generateJavaActualDriver(m);
			}

		} catch (Exception ex) {
			throw new ToolStepException("can't execute:", ex);
		}

	}

	private void generateJavaActualDriver(Model m)
			throws FileNotFoundException, FsmTesterException {

		m.setHasRunControl(true);

		String packageName = m.getSutPackage();

		if (!getPackagePrefix().isEmpty()) {
			packageName += "." + getPackagePrefix();
		}

		File fOutDir = new File(destDirName, packageName.replace(".",
				File.separator));
		fOutDir.mkdirs();

		File fOutFile = new File(fOutDir, m.getSutName() + ".java");

		PrintWriter fOutWriter = new PrintWriter(fOutFile);

		Generator generator = new Generator();

		generator.setModel(m);
		generator.setFout(fOutWriter);
		generator.setAdditionalData(null);
		// generator.setIndenter(null); // TODO which indenter?
		generator.setPackageName(packageName);
		generator
				.setTemplateFileName("eu/fittest/fbk/efsm2ct/plugin/vm/sut-driver.vm");

		generator.run();

	}

	private void generateJavaMockupDriver(Model m) throws IOException,
			FsmTesterException {

		String packageName = m.getSutPackage();

		if (!getPackagePrefix().isEmpty()) {
			packageName += "." + getPackagePrefix();
		}
		
		File fOutDir = new File(destDirName, packageName.replace(".",
				File.separator));
		
		fOutDir.mkdirs();

		File fOutFile = new File(fOutDir, m.getSutName() + ".java");

		PrintWriter fOutWriter = new PrintWriter(fOutFile);

		Generator generator = new Generator();

		generator.setModel(m);
		generator.setFout(fOutWriter);
		generator.setAdditionalData(null);
		// generator.setIndenter(null); // TODO which indenter?
		generator.setPackageName(packageName);
		generator.setTemplateFileName("eu/fittest/fbk/efsm2ct/plugin/vm/mockup-flexstore-java.vm");

		generator.run();

	}

	private void generateJavaMonitor(Model m) throws FileNotFoundException,
			FsmTesterException {

		// Main.runJava(m, packageName, "Test", destDirName, onlyOut,
		// hasRunControl, tmplFileName);

		m.setHasRunControl(true);

		String packageName = m.getSutPackage();

		if (!getPackagePrefix().isEmpty()) {
			packageName += "." + getPackagePrefix();
		}
		
		File fOutDir = new File(destDirName, packageName.replace(".",
				File.separator));
		fOutDir.mkdirs();

		File fOutFile = new File(fOutDir, "Test" + m.getSutName() + ".java");

		PrintWriter fOutWriter = new PrintWriter(fOutFile);

		Generator generator = new Generator();

		generator.setModel(m);
		generator.setFout(fOutWriter);
		generator.setAdditionalData(null);
		// generator.setIndenter(null); // TODO which indenter?
		generator.setPackageName(packageName);
		generator
				.setTemplateFileName("eu/fittest/fbk/efsm2ct/plugin/vm/fsm-sing-int.vm");

		generator.run();

	}

}
