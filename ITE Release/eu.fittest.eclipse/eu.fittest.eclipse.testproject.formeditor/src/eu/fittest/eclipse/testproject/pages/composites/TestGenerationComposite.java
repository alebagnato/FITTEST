package eu.fittest.eclipse.testproject.pages.composites;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

import eu.fittest.eclipse.testproject.pages.TestGenerationPage;
import eu.fittest.eclipse.testproject.pages.composites.testgeneration.CTEFolder;
import eu.fittest.eclipse.testproject.pages.composites.testgeneration.ModelVisitStrategy;
import eu.fittest.eclipse.testproject.pages.composites.testgeneration.ReduceTestSuiteSize;
import eu.fittest.eclipse.testproject.pages.composites.testgeneration.SeleniumBrowserConfig;
import eu.fittest.eclipse.testproject.pages.composites.testgeneration.SeleniumDriverBrowser;
import eu.fittest.eclipse.testproject.pages.composites.testgeneration.SeleniumRemoteHost;
import eu.fittest.eclipse.testproject.pages.composites.testgeneration.SeleniumRemotePort;
import eu.fittest.eclipse.testproject.pages.composites.testgeneration.SourcePackagePrefix;

public class TestGenerationComposite  extends Composite {
	
	private TestGenerationPage page;
	private EObject model;

		public TestGenerationComposite(Section section, int style, FormToolkit toolkit,
			EObject model, TestGenerationPage page) {
		super(section, style);
		this.page = page;
		this.model = model;
		
		createWidgets(toolkit);
	}

	private void createWidgets(FormToolkit toolkit) {
		this.setLayout(new GridLayout(1, false));
		
		Composite elem = new ModelVisitStrategy(this, getStyle(), toolkit, model, page);
		elem.setLayoutData(new GridData(GridData.FILL_BOTH));

		elem = new CTEFolder(this, getStyle(), toolkit, model, page);
		elem.setLayoutData(new GridData(GridData.FILL_BOTH));

		elem = new ReduceTestSuiteSize(this, getStyle(), toolkit, model, page);
		elem.setLayoutData(new GridData(GridData.FILL_BOTH));

		elem = new SourcePackagePrefix(this, getStyle(), toolkit, model, page);
		elem.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		elem = new SeleniumDriverBrowser(this, getStyle(), toolkit, model, page);
		elem.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		elem = new SeleniumRemoteHost(this, getStyle(), toolkit, model, page);
		elem.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		elem = new SeleniumRemotePort(this, getStyle(), toolkit, model, page);
		elem.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		elem = new SeleniumBrowserConfig(this, getStyle(), toolkit, model, page);
		elem.setLayoutData(new GridData(GridData.FILL_BOTH));
	}
}
