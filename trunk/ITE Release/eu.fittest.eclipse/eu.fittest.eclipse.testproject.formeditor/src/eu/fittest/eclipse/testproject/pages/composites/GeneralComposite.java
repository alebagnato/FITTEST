package eu.fittest.eclipse.testproject.pages.composites;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

import eu.fittest.eclipse.testproject.pages.GeneralPage;
import eu.fittest.eclipse.testproject.pages.composites.general.GeneralBaseURL;
import eu.fittest.eclipse.testproject.pages.composites.general.GeneralEntryPage;
import eu.fittest.eclipse.testproject.pages.composites.general.GeneralPHPServerFolder;
import eu.fittest.eclipse.testproject.pages.composites.general.GeneralSUTType;

public class GeneralComposite  extends Composite {
	
	private GeneralPage page;
	private EObject model;

	public GeneralComposite(Composite parent, int style) {
		super(parent, style);
	}

	public GeneralComposite(Section section, int style, FormToolkit toolkit,
			EObject model, GeneralPage generalPage) {
		super(section, style);
		this.page = generalPage;
		this.model = model;
		
		createWidgets(toolkit);
	}

	private void createWidgets(FormToolkit toolkit) {
		this.setLayout(new GridLayout(1, false));
		
		Composite generalName = new GeneralEntryPage(this, SWT.NONE, toolkit, model, page);
		generalName.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		Composite generalBaseURL = new GeneralBaseURL(this, getStyle(), toolkit, model, page);
		generalBaseURL.setLayoutData(new GridData(GridData.FILL_BOTH));

		Composite sutType = new GeneralSUTType(this, getStyle(), toolkit, model, page);
		sutType.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		Composite phpServerFolder = new GeneralPHPServerFolder(this, getStyle(), toolkit, model, page);
		phpServerFolder.setLayoutData(new GridData(GridData.FILL_BOTH));
		
	}

}
