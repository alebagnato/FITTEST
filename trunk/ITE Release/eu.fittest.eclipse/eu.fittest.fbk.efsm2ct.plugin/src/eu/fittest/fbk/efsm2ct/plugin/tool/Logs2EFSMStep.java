package eu.fittest.fbk.efsm2ct.plugin.tool;

import java.io.File;
import java.util.List;

import eu.fittest.fbk.efsm2ct.log2efsm.infer.Inferrer;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.Converter;
import eu.fittest.fbk.efsm2ct.tools.files.FileFinder;

public class Logs2EFSMStep implements ToolStep {

	private File folderFile;
	private File outputFsmFile;
	private String fsmPackage;
	private String fsmName;
	private File filterFile;

	
	public void setFilterFile(File filterFile) {
		this.filterFile = filterFile;
	}

	public File getOutputFsmFile() {
		return outputFsmFile;
	}

	public void setOutputFsmFile(File outputFsmFile) {
		this.outputFsmFile = outputFsmFile;
	}

	public File getFolderFile() {
		return folderFile;
	}

	public void setInputFolderFile(File folderFile) {
		this.folderFile = folderFile;
	}


	public void setFsmPackage(String fsmPackage) {
		this.fsmPackage = fsmPackage;
	}

	public void setFsmName(String fsmName) {
		this.fsmName = fsmName;
	}

	@Override
	public void execute() throws ToolStepException {
		
		FileFinder fu = new FileFinder(".xml");
		
		List<File> logFiles = fu.findFilesRecursively(folderFile);

		Converter converter = new Converter();

		File targetDirectory = outputFsmFile.getParentFile();
		
		File mutatorsDescriptionFile = new File(targetDirectory, "mutators.txt"); // TODO
																					// magic
																					// string
		String outputFileNamePrefix = "log_"; // TODO magic string

		
		try {
			converter.run(logFiles.toArray(new File[0]), mutatorsDescriptionFile, outputFileNamePrefix, targetDirectory, filterFile);
			
			String outputFolder = targetDirectory.getAbsolutePath();
			
			String logFilenamePrefix = outputFileNamePrefix;

			Inferrer inferrer = new Inferrer(fsmPackage, fsmName, outputFolder, outputFsmFile.getAbsolutePath(), mutatorsDescriptionFile.getAbsolutePath(), logFilenamePrefix, null);

			inferrer.infer();
			
			
		} catch (Exception e) {
			throw new ToolStepException("can't execute step",e);
		}
		

		
	}
	
	
}
