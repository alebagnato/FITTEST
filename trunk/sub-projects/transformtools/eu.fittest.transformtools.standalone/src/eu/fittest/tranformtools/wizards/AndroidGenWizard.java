package eu.fittest.tranformtools.wizards;

import java.io.File;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;

import eu.fittest.tranformtools.Activator;

public class AndroidGenWizard extends Wizard {
	private AndroidGenPage paramPage;
	private String cteFile;
	
	public AndroidGenWizard(String cteFile) {
		super();
		this.cteFile = cteFile;
	}

	@Override
	public boolean performFinish() {
		if (paramPage == null) return false;
		return true;
	}

	@Override
	public void addPages() {
		ImageDescriptor imgDes = Activator.getImageDescriptor("icons"
				+ File.separator + "fittest-logo-small.jpg");
		paramPage = new AndroidGenPage("androidGenPage");
		paramPage.setTitle("Generate test cases from a CTE tree");
		paramPage.setDescription("Specifying parameters");
		paramPage.setImageDescriptor(imgDes);

		paramPage.setCteFile(cteFile);
		addPage(paramPage);

	}

	public AndroidGenPage getParamPage() {
		return paramPage;
	}

	public void setParamPage(AndroidGenPage paramPage) {
		this.paramPage = paramPage;
	}

	
	
}
