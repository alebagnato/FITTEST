package eu.fittest.eclipse.testproject.pages.composites.modelinference;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.composites.EMFPropertyStringComposite;
import eu.fittest.test.project.ProjectPackage;

public class AbsFuncDefFile extends EMFPropertyStringComposite {

	public AbsFuncDefFile(Composite parent, int style,
			FormToolkit toolkit, EObject eObject, FEFEMPage page) {
		super(parent, style, toolkit, eObject, page);
	}

	@Override
	protected EStructuralFeature getFeature() {
		return ProjectPackage.eINSTANCE.getModelInferenceType_DomainInputSpecFile();
	}

	@Override
	protected String getLabelText() {
		return "Domain Input Specification File: ";
	}

}
