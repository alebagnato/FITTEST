package eu.fittest.eclipse.testproject.pages.composites.GA;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

import es.cv.gvcase.fefem.common.FEFEMPage;


public class GAComposite extends Composite {
	private FEFEMPage page;
	private EObject model;

		public GAComposite(Section section, int style, FormToolkit toolkit,
			EObject model, FEFEMPage page) {
		super(section, style);
		this.page = page;
		this.model = model;
		
		createWidgets(toolkit);
	}

	private void createWidgets(FormToolkit toolkit) {
		this.setLayout(new GridLayout(1, false));
		
		Composite element = new GAPopulationSize(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));

		element = new GAMutationRate(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));

		element = new GAChromosomeLength(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new GAMaxGen(this, getStyle(), toolkit, model, page);
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new GATimeBudget(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));

		element = new GAStopPort(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
	}
}
