package eu.fittest.eclipse.testproject.pages.composites.oracle;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.composites.EMFPropertyStringComposite;
import eu.fittest.eclipse.testproject.pages.OraclePage;
import eu.fittest.eclipse.testproject.pages.composites.OracleComposite;
import eu.fittest.test.project.ProjectPackage;

public class LloOption extends EMFPropertyStringComposite {

	public LloOption(OracleComposite parent, int style, FormToolkit toolkit,
			EObject model, OraclePage page) {
		super(parent, style, toolkit, model, page);
	}

	@Override
	protected EStructuralFeature getFeature() {
		return ProjectPackage.eINSTANCE.getOracleType_LloOption();
	}

	@Override
	protected String getLabelText() {
		return "LLO options: ";
	}

}