package eu.fittest.tranformtools.wizards;


import java.util.Arrays;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.fbk.se.fsm.visitor.VisitorFactory;
import eu.fittest.tranformtools.Activator;

public class PathPage extends WizardPage {
	private String fsmModel;
	private String domainInputFile;
	private String outputFolder;
	private boolean shouldGenerateTestCase = true;
	private boolean shouldOptimizeTestCase = false;
	
	private String visitorName = VisitorFactory.VISITOR_BREADTHFIRST; // Default
	private String traceFolder; // only needed for *_LOGS visitors
	
	protected PathPage(String pageName) {
		super(pageName);
		
		domainInputFile = Activator.getDefault().getPreferenceStore().getString("PathPage_domainInputFile");
		outputFolder =  Activator.getDefault().getPreferenceStore().getString("PathPage_outputFolder");
		visitorName =  Activator.getDefault().getPreferenceStore().getString("PathPage_visitorName");
		traceFolder =  Activator.getDefault().getPreferenceStore().getString("PathPage_traceFolder");
	}

	
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);
		
		Label label = new Label(composite, SWT.NONE);
		label.setText("FSM model: ");
  		final Text fsmModelText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		fsmModelText.setText(fsmModel);
		fsmModelText.setEditable(false);
		
		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		fsmModelText.setLayoutData(layoutData);
		
		
		WUtil.createSpace(composite);
		// Second row, output folder
		
		label = new Label(composite, SWT.NONE);
		label.setText("Output folder:");
		final Text outputPath = new Text(composite, SWT.BORDER | SWT.SINGLE);
		outputPath.setText(outputFolder);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		outputPath.setLayoutData(layoutData);
		
		Button outputBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		outputBrowse.setText("  Browse...  ");
		outputBrowse.setLayoutData(layoutData);
		
		outputBrowse.addMouseListener(new MouseListener(){

			
			public void mouseDoubleClick(MouseEvent e) {
			}

			
			public void mouseDown(MouseEvent e) {
				/*
				DirectoryDialog directoryDialog = new DirectoryDialog(getShell());
				String tmp = directoryDialog.open();
				if (tmp != null){
					outputPath.setText(tmp);
					outputFolder = tmp;
					
					if (!troposModel.equals("") && !outputFolder.equals("")){
						setPageComplete(true);
					}
				}
				*/
				
				String tmp = WUtil.getSelection(getShell(), "Output Folder", "Select a folder for CTE trees");
				if (!tmp.equals("")){ 
					outputPath.setText(tmp);
					outputFolder = tmp;
					
					Activator.getDefault().getPreferenceStore().setValue("PathPage_outputFolder", outputFolder);
					
					if (!fsmModel.equals("") && !outputFolder.equals("")){
						setPageComplete(true);
					}
				}
			}

			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		WUtil.createSpace(composite);
		// Ask user to use built-in protocol templates 
		
//		label = new Label(composite, SWT.NONE);
//		label.setText("Domain input specification");
//		
//		final Button hasDomainInputs = new Button(composite, SWT.CHECK);
//		
//		layoutData = new GridData(GridData.FILL_HORIZONTAL);
//		layoutData.horizontalSpan = 2;
//		hasDomainInputs.setText("Yes");
//		hasDomainInputs.setLayoutData(layoutData);
//		
		
		label = new Label(composite, SWT.NONE);
		label.setText("Domain input specification");
		
		final Text domainInputText = new Text(composite, SWT.BORDER);
		domainInputText.setText(domainInputFile);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		domainInputText.setLayoutData(layoutData);
		
		domainInputText.addModifyListener(new ModifyListener() {
			
			public void modifyText(ModifyEvent e) {
				domainInputFile = domainInputText.getText();
				
			}
		});
		
		final Button domainInputBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		domainInputBrowse.setText("  Browse...  ");
		domainInputBrowse.setLayoutData(layoutData);
		
//		domainInputBrowse.setEnabled(false);
//		domainInputText.setEnabled(false);
		
		domainInputBrowse.addMouseListener(new MouseListener(){

			
			public void mouseDoubleClick(MouseEvent e) {
			}

			
			public void mouseDown(MouseEvent e) {
				
				//String tmp = getSelection("Scenario templates list", "Select the file that contains scenario templates");
				
				//DirectoryDialog directoryDialog = new DirectoryDialog(getShell());
				FileDialog fileDialog = new FileDialog(getShell());
				fileDialog.setFilterExtensions(new String[]{"*.xml"});
				fileDialog.setFileName(domainInputText.getText());
				fileDialog.setFilterPath(domainInputFile);
				String tmp = fileDialog.open();
				if (tmp != null){
					domainInputText.setText(tmp);
					domainInputFile = tmp;
					Activator.getDefault().getPreferenceStore().setValue("PathPage_domainInputFile", tmp);
//					ScenariosPage nextPage = (ScenariosPage) getNextPage();
//					nextPage.setPath(tmp);
//					nextPage.update();
				}
			}

			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		// Visitor Name
		label = new Label(composite, SWT.NONE);
		label.setText("Path generation strategy");
		
		final Combo visitorSelect = new Combo(composite, SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		visitorSelect.setLayoutData(layoutData);
		String[] visitorNames = new String[VisitorFactory.VISITORS.size()];
		int i = 0;
		for (String s : VisitorFactory.VISITORS.keySet()){
			visitorNames[i++] = s;
		}
		
		Arrays.sort(visitorNames);
		
		visitorSelect.setItems(visitorNames);
		
		visitorSelect.select(0);
		for (i = 0; i < visitorNames.length; i++){
			if (visitorNames[i].equals(visitorName)){
				visitorSelect.select(i);
				break;
			}
		}
		
		WUtil.createSpace(composite);
		// Second row, output folder
		
		label = new Label(composite, SWT.NONE);
		label.setText("Trace folder:");
		final Text traceFolderPath = new Text(composite, SWT.BORDER | SWT.SINGLE);
		traceFolderPath.setText(traceFolder);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		traceFolderPath.setLayoutData(layoutData);
		
		final Button traceFolderBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		traceFolderBrowse.setText("  Browse...  ");
		traceFolderBrowse.setLayoutData(layoutData);
		
		traceFolderPath.setEnabled(false);
		traceFolderBrowse.setEnabled(false);
		
		traceFolderBrowse.addMouseListener(new MouseListener(){
			public void mouseDoubleClick(MouseEvent e) {
			}
			
			public void mouseDown(MouseEvent e) {
				DirectoryDialog directoryDialog = new DirectoryDialog(getShell());
				String tmp = directoryDialog.open();
				if (tmp != null){
					traceFolderPath.setText(tmp);
					traceFolder = tmp;
					Activator.getDefault().getPreferenceStore().setValue("PathPage_traceFolder", traceFolder);
				}
			}
			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		visitorSelect.addSelectionListener(new SelectionListener() {

			public void widgetSelected(SelectionEvent e) {
				visitorName = visitorSelect.getText();
				Activator.getDefault().getPreferenceStore().setValue("PathPage_visitorName", visitorName);
				String visitorValue = VisitorFactory.VISITORS.get(visitorName);
				if (visitorValue.equals(VisitorFactory.VISITOR_COVERAGE_UNIFORM_LOGS)
						|| visitorValue.equals(VisitorFactory.VISITOR_COVERAGEWITHFRQ_LOGS)
						|| visitorValue.equals(VisitorFactory.VISITOR_xLeastFrequentEven_LOGS)){
					traceFolderPath.setEnabled(true);
					traceFolderBrowse.setEnabled(true);
				} else {
					traceFolderPath.setEnabled(false);
					traceFolderBrowse.setEnabled(false);
				}
				getContainer().updateButtons();
			}
			
			
			public void widgetDefaultSelected(SelectionEvent e) {
				visitorName = visitorSelect.getText();
				Activator.getDefault().getPreferenceStore().setValue("PathPage_visitorName", visitorName);
			}
		});		
		
		
		WUtil.createSpace(composite);
		// asking if the user want to generate test cases
		label = new Label(composite, SWT.NONE);
		label.setText("Generate test cases (2-way)");
		
		final Button generateTestCases = new Button(composite, SWT.CHECK);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		generateTestCases.setText("Check to say Yes");
		generateTestCases.setLayoutData(layoutData);
		generateTestCases.setSelection(true);
		
		generateTestCases.addMouseListener(new MouseListener() {
			
			public void mouseUp(MouseEvent e) {
				shouldGenerateTestCase = generateTestCases.getSelection();
			}
			
			public void mouseDown(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			public void mouseDoubleClick(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
		});
		
		
		WUtil.createSpace(composite);
		// asking if the user want to generate test cases
		label = new Label(composite, SWT.NONE);
		label.setText("Perform global-pairwise optimization");
		
		final Button optimizeTestCases = new Button(composite, SWT.CHECK);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		optimizeTestCases.setText("Check to say Yes");
		optimizeTestCases.setLayoutData(layoutData);
		optimizeTestCases.setSelection(false);
		
		optimizeTestCases.addMouseListener(new MouseListener() {
			
			public void mouseUp(MouseEvent e) {
				shouldOptimizeTestCase = optimizeTestCases.getSelection();
			}
			
			public void mouseDown(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			public void mouseDoubleClick(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
		});
		
		setControl(composite);
		return;
	}
	

	public String getFsmModel() {
		return fsmModel;
	}

	public void setFsmModel(String fsmModel) {
		this.fsmModel = fsmModel;
	}

	public String getDomainInputFile() {
		return domainInputFile;
	}

	public void setDomainInputFile(String domainInputFile) {
		this.domainInputFile = domainInputFile;
	}

	public String getOutputFolder() {
		return outputFolder;
	}

	public void setOutputFolder(String outputFolder) {
		this.outputFolder = outputFolder;
	}


	public boolean isShouldGenerateTestCase() {
		return shouldGenerateTestCase;
	}


	public boolean isShouldOptimizeTestCase() {
		return shouldOptimizeTestCase;
	}


	public String getVisitorName() {
		return visitorName;
	}


	public void setVisitorName(String visitorName) {
		this.visitorName = visitorName;
	}


	public String getTraceFolder() {
		return traceFolder;
	}


	public void setTraceFolder(String traceFolder) {
		this.traceFolder = traceFolder;
	}
	
	
}
