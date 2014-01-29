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
import eu.fittest.eclipse.testproject.pages.composites.LoggingInstrumentationComposite;
import eu.fittest.eclipse.testproject.pages.composites.LoggingTargetComposite;
import eu.fittest.test.project.impl.DocumentRootImpl;

public class LoggingPage extends FEFEMPage {
	
	private static final String PAGE_ID = "eu.fittest.eclipse.testproject.formeditor.Logging";
	
	private Composite pageComposite;
	private LoggingTargetComposite loggingComposite;
	private LoggingInstrumentationComposite instrumentationComposite;

	public LoggingPage(FormEditor editor) {
		super(editor, PAGE_ID, "Logging");
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
//		layout.numColumns = 1;
//		layout.makeColumnsEqualWidth = true;
		
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
		
		Section section = this.createSection(pageComposite, toolkit, "Logging Settings", null);
//		Section section = toolkit.createSection(pageComposite, Section.TWISTIE | Section.EXPANDED);
//		section.setText("Logging Settings");
		
		GridData td = new GridData(GridData.FILL_BOTH);
		section.setLayoutData(td);
//		toolkit.createCompositeSeparator(section);
		
		EObject rootModel = getEditor().getModel();
		EObject loggingModel = ((DocumentRootImpl)rootModel).getTestProject().getLogging().getLogTarget();
		
		loggingComposite = new LoggingTargetComposite(section, SWT.NONE, toolkit, loggingModel, this);
		
		GridData gd0 = new GridData(GridData.FILL_BOTH);
		loggingComposite.setLayoutData(gd0);
		
		toolkit.paintBordersFor(loggingComposite);
		section.setClient(loggingComposite);
		section.layout();

		
//		Section instrumentSection = this.createSection(pageComposite, toolkit, "Instrumentation Settings", null);
		Section instrumentSection = toolkit.createSection(pageComposite, Section.TWISTIE | Section.COMPACT);
		instrumentSection.setText("Instrumentation Settings");
		td = new GridData(GridData.FILL_BOTH);
		instrumentSection.setLayoutData(td);
		toolkit.createCompositeSeparator(instrumentSection);
		
		EObject instrumentationModel = ((DocumentRootImpl)rootModel).getTestProject().getLogging().getInstrumentation();
		instrumentationComposite = new LoggingInstrumentationComposite(instrumentSection, SWT.NONE, toolkit, instrumentationModel, this);
		
		gd0 = new GridData(GridData.FILL_BOTH);
		instrumentationComposite.setLayoutData(gd0);
		
		instrumentSection.setClient(instrumentationComposite);
		instrumentSection.layout();
	}



	@Override
	public void refresh() {
		
	}

}
