package eu.fittest.eclipse.testproject.pages.composites.general;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.composites.EMFPropertyStringComposite;
import eu.fittest.test.project.ProjectPackage;

public class GeneralBaseURL extends EMFPropertyStringComposite {

	public GeneralBaseURL(Composite parent, int style, FormToolkit toolkit,
			EObject model, FEFEMPage page) {
		super(parent, style, toolkit, model, page);
	}

	@Override
	protected EStructuralFeature getFeature() {
		return ProjectPackage.eINSTANCE.getGeneralType_BaseURL(); 
	}

	@Override
	protected String getLabelText() {
		return "Base URL: ";
	}

}
