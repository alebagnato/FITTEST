package eu.fittest.eclipse.testproject.pages.composites.logging;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.composites.EMFPropertyIntegerComposite;
import eu.fittest.test.project.ProjectPackage;

public class LogLevel extends EMFPropertyIntegerComposite {

	public LogLevel(Composite parent, int style, FormToolkit toolkit,
			EObject object, FEFEMPage page) {
		super(parent, style, toolkit, object, page);
		
		getSpinner().setMinimum(1);
		getSpinner().setMaximum(5);
	}

	@Override
	protected EStructuralFeature getFeature() {
		return ProjectPackage.eINSTANCE.getLogTargetType_LogLevel();
	}

	@Override
	protected String getLabelText() {
		return "Log level (1-5): ";
	}
}
