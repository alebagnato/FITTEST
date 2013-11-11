package eu.fittest.tranformtools.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import eu.fittest.project.config.TestProject;

public class AllInOneLog2TestPage extends WizardPage {

//	private TestProject activeProjectConfig;
	
	private boolean modelInferenceSelected = true;
	private boolean xinputMiningSelected = true;
	private boolean cteGenSelected = true;
	private boolean selenGenSelected = true;
	
	public AllInOneLog2TestPage(String pageName, TestProject testProjectConfig) {
		super(pageName);
//		this.activeProjectConfig = testProjectConfig;
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(2, false);
		composite.setLayout(layout);

		// FSM
		Label label = new Label(composite, SWT.NONE);
		label.setText("1. Infer FSM model: ");
		GridData layoutData = new GridData();
		label.setLayoutData(layoutData);
		
		final Button fsmTask = new Button(composite, SWT.CHECK);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		fsmTask.setLayoutData(layoutData);
		fsmTask.setSelection(true);
		fsmTask.addMouseListener(new MouseListener() {
			
			@Override
			public void mouseUp(MouseEvent e) {
				modelInferenceSelected = fsmTask.getSelection();
			}
			
			@Override
			public void mouseDown(MouseEvent e) {
				
			}
			
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				
			}
		});

		// Xinput
		label = new Label(composite, SWT.NONE);
		label.setText("2. Mine Domain Input Specifications: ");
		layoutData = new GridData();
		label.setLayoutData(layoutData);
		
		final Button xinputTask = new Button(composite, SWT.CHECK);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		xinputTask.setLayoutData(layoutData);
		xinputTask.setSelection(true);
		xinputTask.addMouseListener(new MouseListener() {
			
			@Override
			public void mouseUp(MouseEvent e) {
				xinputMiningSelected = xinputTask.getSelection();
			}
			
			@Override
			public void mouseDown(MouseEvent e) {
				
			}
			
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				
			}
		});
		WUtil.createSpace(composite);
		
		// Generate CTEs
		label = new Label(composite, SWT.NONE);
		label.setText("3. Generate CT trees: ");
		layoutData = new GridData();
		label.setLayoutData(layoutData);
		
		final Button cteTask = new Button(composite, SWT.CHECK);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		cteTask.setLayoutData(layoutData);
		cteTask.setSelection(true);
		
		cteTask.addMouseListener(new MouseListener() {
			
			@Override
			public void mouseUp(MouseEvent e) {
				cteGenSelected = cteTask.getSelection();
			}
			
			@Override
			public void mouseDown(MouseEvent e) {}
			
			@Override
			public void mouseDoubleClick(MouseEvent e) {}
		});
	
		// Generate Selenium Test Cases
	
		label = new Label(composite, SWT.NONE);
		label.setText("4. Generate tests: ");
		layoutData = new GridData();
		label.setLayoutData(layoutData);
		
		final Button seleniumTask = new Button(composite, SWT.CHECK);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		seleniumTask.setLayoutData(layoutData);
		seleniumTask.setSelection(true);
		seleniumTask.addMouseListener(new MouseListener() {
			
			@Override
			public void mouseUp(MouseEvent e) {
				selenGenSelected = seleniumTask.getSelection();
			}
			
			@Override
			public void mouseDown(MouseEvent e) {}
			
			@Override
			public void mouseDoubleClick(MouseEvent e) {}
		});
		
		// Remark
		WUtil.createSpace(composite);
		label = new Label(composite, SWT.NONE);
		label.setText("Remark: the parameters in the project configuration will be used for these tasks.");
		layoutData = new GridData();
		layoutData.horizontalSpan = 2;
		label.setLayoutData(layoutData);
		
		setControl(composite);
	}

	public boolean isModelInferenceSelected() {
		return modelInferenceSelected;
	}

	public boolean isXinputMiningSelected() {
		return xinputMiningSelected;
	}

	public boolean isCTEGenSelected() {
		return cteGenSelected;
	}

	public boolean isSelenGenSelected() {
		return selenGenSelected;
	}

}
