package eu.fittest.tranformtools.wizards;

import java.io.File;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;

import eu.fittest.tranformtools.Activator;

public class FSMGenWizard extends Wizard {

	private FSMPage paramPage = null;
	
	public boolean performFinish() {
		if (paramPage == null) return false;
		return true;
	}

	
	public void addPages() {
		ImageDescriptor imgDes = Activator.getImageDescriptor("icons" + File.separator + "fittest-logo-small.jpg");
		paramPage = new FSMPage("Specify input / output");
		paramPage.setDescription("Specify log folder and output file to store inferred model");
		paramPage.setImageDescriptor(imgDes);
		
		addPage(paramPage);
	}

	public FSMPage getParamPage() {
		return paramPage;
	}
	
}
