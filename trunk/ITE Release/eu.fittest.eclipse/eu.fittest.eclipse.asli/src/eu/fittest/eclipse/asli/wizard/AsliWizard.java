package eu.fittest.eclipse.asli.wizard;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.wizard.Wizard;


public class AsliWizard extends Wizard {

	AsliPage _asliPage = null;
	IFile _specification = null;
	
	public AsliWizard(IFile specification) {
		super();
		_specification = specification;
	}

	public void addPages() {
		_asliPage = new AsliPage();
		_asliPage.setSpecification(_specification.getLocation().toOSString());	
		addPage(_asliPage);
	}
	
	
	public boolean performFinish() {
		return true;
	}
	
	public AsliPage getParameterPage(){
		return _asliPage;
	}

}
