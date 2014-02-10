package eu.fittest.eclipse.testproject.pages.composites;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

import eu.fittest.eclipse.testproject.pages.OraclePage;
import eu.fittest.eclipse.testproject.pages.composites.oracle.GHCRTops;
import eu.fittest.eclipse.testproject.pages.composites.oracle.IncludedEvents;
import eu.fittest.eclipse.testproject.pages.composites.oracle.IncludedFields;
import eu.fittest.eclipse.testproject.pages.composites.oracle.IncludedFunctions;
import eu.fittest.eclipse.testproject.pages.composites.oracle.LloOption;
import eu.fittest.eclipse.testproject.pages.composites.oracle.OracleFile;
import eu.fittest.eclipse.testproject.pages.composites.oracle.ReportFile;
import eu.fittest.eclipse.testproject.pages.composites.oracle.ViolationFile;

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
		
		Composite element = new GHCRTops(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new OracleFile(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new ReportFile(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new ViolationFile(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new IncludedEvents(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new IncludedFields(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new IncludedFunctions(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		element = new LloOption(this, getStyle(), toolkit, model, page); 
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
		
	}

}
