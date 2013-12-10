package eu.fittest.tranformtools.wizards;

import java.io.File;

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;

import eu.fittest.tranformtools.Activator;

public class FSMGenWizard extends Wizard {

	private FSMPage paramPage = null;
	private IFolder _input;
	private IFolder _output;
	private String _outputFile;
	private boolean _isGenDotFile = false;
	
	public String getInput(){
		return _input.getLocation().toOSString();
	}
	
	public String getOutput(){
		return _outputFile;
	}
	
	public FSMGenWizard() {
		this(null,null);
	}
	
	public FSMGenWizard(IFolder input, IFolder output){
		_input = input;
		_output = output;
	}
	
	
	public boolean performFinish() {
		if (paramPage == null) return false;
		
		File outputFile = _output.getFile(paramPage.getOutputModelFile()).getLocation().toFile();
		if(!outputFile.getParentFile().exists()){
			outputFile.getParentFile().mkdirs();
		}
		_outputFile = outputFile.getAbsolutePath();
		_isGenDotFile = paramPage.isGenDotFile();
		return true;
	}

	
	public void addPages() {
		ImageDescriptor imgDes = Activator.getImageDescriptor("icons" + File.separator + "fittest-logo-small.jpg");
		String sessionName = _input.getParent().getParent().getName().toLowerCase();
		paramPage = new FSMPage("Specify input / output",sessionName+"/"+sessionName+".fsm");//logging component folder -> agent component folder -> recording session folder
		
		paramPage.setDescription("Specify log folder and output file to store inferred model");
		paramPage.setImageDescriptor(imgDes);
		
		addPage(paramPage);
	}

	public boolean isGenDotFile() {
		return _isGenDotFile;
	}
	
}
