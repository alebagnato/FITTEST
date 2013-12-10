package eu.fittest.fbk.efsm2ct.plugin.wizards.log2efsm;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.wizard.Wizard;

import eu.fittest.fbk.efsm2ct.plugin.ConfigurationFactory;

public class Logs2EFSMGenerationWizard extends Wizard {

	// private static Logger logger = Logger.getAnonymousLogger();

	private Logs2EFSMGenerationPage paramPage = null;
	private IFolder input;
	private IFolder output;
	private String outputFilePath;
	private String userFileString;
	private boolean isGenDotFile = false;

	public String getInput() {
		return input.getLocation().toOSString();
	}

	public String getOutput() {
		return outputFilePath;
	}

//	public GenerationWizard() {
//		this(null, null);
//	}
	
	public String getUserFileString() {
		return userFileString;
	}

	public Logs2EFSMGenerationWizard(IFolder input, IFolder output) {

		// logger.info("FSMGenWizard constructor called:" + input + " " + output);

		this.setWindowTitle("Inferring an EFSM from log files");
		
		this.input = input;
		this.output = output;
	}

	public boolean performFinish() {
		
		if (paramPage == null) {
			return false;
		}
		
		userFileString = paramPage.getOutputModelFile();

		IFile modelIFile = output.getFile(userFileString);
		
		File outputFile = modelIFile.getLocation().toFile();
		
		if (!outputFile.getParentFile().exists()) {
			outputFile.getParentFile().mkdirs();
		}
		
		outputFilePath = outputFile.getAbsolutePath();	
		isGenDotFile = paramPage.isGenDotFile();
		
		return true;
	}

	public void addPages() {
//		ImageDescriptor imgDes = Activator.getImageDescriptor("icons" + File.separator + "fittest-logo-small.jpg");
//		String sessionName = _input.getParent().getParent().getName().toLowerCase();
//		paramPage = new FSMPage("Specify input / output", sessionName + "/" + sessionName + ".fsm");// logging
//																									// component
//																									// folder
//																									// ->
//																									// agent
//																									// component
//																									// folder
//																									// ->
//																									// recording
//																									// session
//																									// folder
//
//		paramPage.setDescription("Specify log folder and output file to store inferred model");
//		paramPage.setImageDescriptor(imgDes);


		// String outputFilename = ConfigurationFactory.getInstance().getDefaultModelFilename();
		paramPage = new Logs2EFSMGenerationPage("Save the inferred model in file:", "YourSut.efsm");
		
		addPage(paramPage);
		
	}

	public boolean isGenDotFile() {
		return isGenDotFile;
	}

}
