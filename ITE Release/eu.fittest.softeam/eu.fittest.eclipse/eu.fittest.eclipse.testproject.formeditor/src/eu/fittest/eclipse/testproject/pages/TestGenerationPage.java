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
import eu.fittest.eclipse.testproject.pages.composites.TestGenerationComposite;
import eu.fittest.eclipse.testproject.pages.composites.GA.GAComposite;
import eu.fittest.test.project.TestGenerationType;
import eu.fittest.test.project.impl.DocumentRootImpl;

public class TestGenerationPage extends FEFEMPage {
	
	private static final String PAGE_ID = "eu.fittest.eclipse.testproject.formeditor.TestGeneration";
	
	private Composite pageComposite;
	private TestGenerationComposite testGenearationComposite;

	public TestGenerationPage(FormEditor editor) {
		super(editor, PAGE_ID, "Test Generation");
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
		
		Section section = this.createSection(pageComposite, toolkit, "Test Generation Settings", null);
		
		EObject rootModel = getEditor().getModel();
		EObject testGenModel = ((DocumentRootImpl)rootModel).getTestProject().getTestGeneration();
		
		testGenearationComposite = new TestGenerationComposite(section, SWT.NONE, toolkit, testGenModel, this);
		
		GridData td = new GridData(GridData.FILL_BOTH);
		testGenearationComposite.setLayoutData(td);
		
		section.setClient(testGenearationComposite);
		section.layout();
		
		// GA section
		Section gaSection = toolkit.createSection(pageComposite, Section.TWISTIE | Section.COMPACT);
		gaSection.setText("GA Settings for Evosuite");
		td = new GridData(GridData.FILL_BOTH);
		gaSection.setLayoutData(td);
		toolkit.createCompositeSeparator(gaSection);
		
		EObject gaParamModel = ((TestGenerationType) testGenModel).getGaParam();
		if (gaParamModel != null){
			GAComposite gaComposite = new GAComposite(gaSection, SWT.NONE, toolkit, gaParamModel, this);
			td = new GridData(GridData.FILL_BOTH);
			gaComposite.setLayoutData(td);
			gaSection.setClient(gaComposite);
			gaSection.layout();
		}
		
		
	}



	@Override
	public void refresh() {
		
	}

}
