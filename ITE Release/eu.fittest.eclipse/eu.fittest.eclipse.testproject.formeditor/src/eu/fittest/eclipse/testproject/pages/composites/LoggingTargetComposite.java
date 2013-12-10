package eu.fittest.eclipse.testproject.pages.composites;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

import eu.fittest.eclipse.testproject.pages.LoggingPage;
import eu.fittest.eclipse.testproject.pages.composites.logging.LogLevel;
import eu.fittest.eclipse.testproject.pages.composites.logging.LogTargetStoreDir;

public class LoggingTargetComposite  extends Composite {
	
	private LoggingPage page;
	private EObject model;

		public LoggingTargetComposite(Section section, int style, FormToolkit toolkit,
			EObject model, LoggingPage loggingPage) {
		super(section, style);
		this.page = loggingPage;
		this.model = model;
		
		createWidgets(toolkit);
	}

	private void createWidgets(FormToolkit toolkit) {
		this.setLayout(new GridLayout(1, false));
		
		Composite element = new LogLevel(this, getStyle(), toolkit, model, page);
		element.setLayoutData(new GridData(GridData.FILL_BOTH));

		element = new LogTargetStoreDir(this, getStyle(), toolkit, model, page);
		element.setLayoutData(new GridData(GridData.FILL_BOTH));
	}

}
