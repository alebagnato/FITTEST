package eu.fittest.eclipse.testproject.pages;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;

import es.cv.gvcase.fefem.common.FEFEMPage;
import eu.fittest.eclipse.testproject.pages.composites.OracleComposite;
import eu.fittest.test.project.impl.DocumentRootImpl;

public class OraclePage extends FEFEMPage {
	
	private static final String PAGE_ID = "eu.fittest.eclipse.testproject.formeditor.Oracle";
	
	private Composite pageComposite;
	private OracleComposite oracleComposite;

	public OraclePage(FormEditor editor) {
		super(editor, PAGE_ID, "Oracle");
	}

	@Override
	protected void createFormContent(IManagedForm managedForm) {
		ScrolledForm form = managedForm.getForm();
		toolkit = managedForm.getToolkit();
		form.setText(this.getTitle()); //$NON-NLS-1$
		toolkit.decorateFormHeading(form.getForm());

		GridLayout layout = new GridLayout();
		layout.marginLeft = 10;
		layout.marginRight = 10;
		layout.marginTop = 5;
		layout.numColumns = 1;
		layout.makeColumnsEqualWidth = true;
		
		form.getBody().setLayout(layout);

		GridLayout columnLayout = new GridLayout();
		columnLayout.numColumns = 1;
		
		pageComposite = toolkit.createComposite(form.getBody());
		
		GridData layoutData = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING
				| GridData.VERTICAL_ALIGN_BEGINNING | GridData.VERTICAL_ALIGN_FILL
				| GridData.HORIZONTAL_ALIGN_FILL);

		layoutData.grabExcessHorizontalSpace = true;
		
		pageComposite.setLayout(columnLayout);
		pageComposite.setLayoutData(layoutData);
		
		Section section = this.createSection(pageComposite, toolkit, "Oracle Settings", null);
		
		EObject rootModel = getEditor().getModel();
		EObject oracleModel = ((DocumentRootImpl)rootModel).getTestProject().getOracle();
		
		oracleComposite = new OracleComposite(section, SWT.NONE, toolkit, oracleModel, this);
		
		GridData gd0 = new GridData(GridData.FILL_BOTH);
		oracleComposite.setLayoutData(gd0);
		
		section.setClient(oracleComposite);
		section.layout();
	}



	@Override
	public void refresh() {
		
	}

}
