package eu.fittest.eclipse.testproject.pages.composites;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

import eu.fittest.eclipse.testproject.pages.ModelInferencePage;
import eu.fittest.eclipse.testproject.pages.composites.modelinference.AbsFuncDefFile;
import eu.fittest.eclipse.testproject.pages.composites.modelinference.DomainInputSpecFile;
import eu.fittest.eclipse.testproject.pages.composites.modelinference.GenerateDOTFile;
import eu.fittest.eclipse.testproject.pages.composites.modelinference.InferenceTechnique;
import eu.fittest.eclipse.testproject.pages.composites.modelinference.ModelFile;

public class ModelInferenceComposite  extends Composite {
	
	private ModelInferencePage page;
	private EObject model;

		public ModelInferenceComposite(Section section, int style, FormToolkit toolkit,
			EObject model, ModelInferencePage modelInferencePage) {
		super(section, style);
		this.page = modelInferencePage;
		this.model = model;
		
		createWidgets(toolkit);
	}

	private void createWidgets(FormToolkit toolkit) {
		this.setLayout(new GridLayout(1, false));
		
		Composite element = new InferenceTechnique(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new ModelFile(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));

		element = new DomainInputSpecFile(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new AbsFuncDefFile(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new GenerateDOTFile(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
	}

}
