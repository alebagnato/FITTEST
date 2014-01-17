package eu.fittest.eclipse.testproject.pages.composites;

import org.eclipse.swt.layout.GridLayout;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

import eu.fittest.eclipse.testproject.pages.OraclePage;

public class OracleComposite  extends Composite {
	
	private OraclePage page;
	private EObject model;

		public OracleComposite(Section section, int style, FormToolkit toolkit,
			EObject model, OraclePage page) {
		super(section, style);
		this.page = page;
		this.model = model;
		
		createWidgets(toolkit);
	}

	private void createWidgets(FormToolkit toolkit) {
		this.setLayout(new GridLayout(1, false));
		
		
	}

}
