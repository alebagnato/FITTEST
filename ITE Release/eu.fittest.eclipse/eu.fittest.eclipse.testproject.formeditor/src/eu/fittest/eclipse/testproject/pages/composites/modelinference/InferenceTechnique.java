package eu.fittest.eclipse.testproject.pages.composites.modelinference;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.composites.EMFPropertyEEnumComposite;
import eu.fittest.test.project.ProjectPackage;

public class InferenceTechnique extends EMFPropertyEEnumComposite {

	public InferenceTechnique(Composite parent, int style, FormToolkit toolkit,
			EObject object, FEFEMPage page) {
		super(parent, style, toolkit, object, page);
	}

	@Override
	protected EStructuralFeature getFeature() {
		return ProjectPackage.eINSTANCE.getModelInferenceType_InferenceTechnique();
	}

	@Override
	protected String getLabelText() {
		return "Model Inference Technique: ";
	}

}
