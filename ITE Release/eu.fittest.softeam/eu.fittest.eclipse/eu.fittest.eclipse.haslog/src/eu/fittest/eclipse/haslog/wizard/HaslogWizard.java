package eu.fittest.eclipse.haslog.wizard;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.wizard.Wizard;


public class HaslogWizard extends Wizard {

	HaslogPage _haslogPage = null;
	IFile _specification = null;
	
	public HaslogWizard(IFile specification) {
		super();
		_specification = specification;
	}
                                                    
	public void addPages() {
		_haslogPage = new HaslogPage();
		_haslogPage.setSpecification(_specification.getLocation().toOSString());	
		addPage(_haslogPage);
	}
	
	
	public boolean performFinish() {
		return true;
	}
	
	public HaslogPage getParameterPage(){
		return _haslogPage;
	}

}
