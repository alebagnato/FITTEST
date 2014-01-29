package eu.fittest.eclipse.testproject.pages.composites.logging;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.composites.EMFPropertyStringComposite;
import eu.fittest.test.project.ProjectPackage;

public class LogInstrumentationGHCRTOption extends EMFPropertyStringComposite {

	public LogInstrumentationGHCRTOption(Composite parent, int style,
			FormToolkit toolkit, EObject eObject, FEFEMPage page) {
		super(parent, style, toolkit, eObject, page);
	}

	@Override
	protected EStructuralFeature getFeature() {
		return ProjectPackage.eINSTANCE.getInstrumentationType_GhcrtOption();
	}

	@Override
	protected String getLabelText() {
		return "Additional GHC run-time options: ";
	}

}
