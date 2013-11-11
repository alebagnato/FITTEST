package eu.fittest.eclipse.testproject.pages.composites.modelinference;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.composites.EMFPropertyBooleanComposite;
import eu.fittest.test.project.ProjectPackage;

public class GenerateDOTFile extends EMFPropertyBooleanComposite {

	public GenerateDOTFile(Composite parent, int style, FormToolkit toolkit,
			EObject object, FEFEMPage page) {
		super(parent, style, toolkit, object, page);
	}

	@Override
	protected EStructuralFeature getFeature() {
		return ProjectPackage.eINSTANCE.getModelInferenceType_GenerateDot(); 
	}

	@Override
	protected String getLabelText() {
		return "Generate DOT file";
	}

}
