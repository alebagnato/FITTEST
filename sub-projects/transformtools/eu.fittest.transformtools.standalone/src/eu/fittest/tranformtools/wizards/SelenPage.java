package eu.fittest.tranformtools.wizards;

import java.util.ArrayList;

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

public class SelenPage extends WizardPage {
	private String cteFile;
	private String packageName;
	private String className;
	private String targetPage;
	private String seleniumDriver;
	private String domainInputFile;
	private String outputFolder;
	private boolean genNewProject = true;
	private boolean onlyValidTestCase = true;
	
	protected SelenPage(String pageName) {
		super(pageName);
		
		packageName = StorePreferences.getSavedValue("SelenPage_packageName");
		className = StorePreferences.getSavedValue("SelenPage_className");
		targetPage = StorePreferences.getSavedValue("SelenPage_targetPage");
		seleniumDriver = StorePreferences.getSavedValue("SelenPage_seleniumDriver");
		domainInputFile = StorePreferences.getSavedValue("SelenPage_domainInputFile");
		outputFolder = StorePreferences.getSavedValue("SelenPage_outputFolder");
	}

	
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);
		
		// CTE file
		Label label = new Label(composite, SWT.NONE);
		label.setText("CTE file: ");
  		final Text cteFileText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		cteFileText.setText(cteFile);
		
		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		cteFileText.setLayoutData(layoutData);
		
		Button cteFileBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		cteFileBrowse.setText("  Browse..  ");
		cteFileBrowse.setLayoutData(layoutData);
		
		cteFileBrowse.addMouseListener(new MouseListener(){

			public void mouseDoubleClick(MouseEvent e) {
			}

			public void mouseDown(MouseEvent e) {
				
				String tmp = WUtil.getSelection(getShell(), "CTE File", "Select a CTE file");
				if (!tmp.equals("")){ 
					cteFileText.setText(tmp);
					cteFile = tmp;
				}
			}

			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		WUtil.createSpace(composite);
		
		// New project or output folder
		label = new Label(composite, SWT.NONE);
		label.setText("Generate a new Eclipse project");
		
		final Button createNewProject = new Button(composite, SWT.CHECK);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		createNewProject.setText("Yes");
		createNewProject.setLayoutData(layoutData);
		createNewProject.setSelection(true);
		
		// Output folder
		label = new Label(composite, SWT.NONE);
		label.setText("Output folder:");
		
		final Text outputPath = new Text(composite, SWT.BORDER | SWT.SINGLE);
		outputPath.setText(outputFolder);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		outputPath.setLayoutData(layoutData);
		
		final Button outputBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		outputBrowse.setText("  Browse...  ");
		outputBrowse.setLayoutData(layoutData);
		
		outputBrowse.addMouseListener(new MouseListener(){

			public void mouseDoubleClick(MouseEvent e) {
			}

			public void mouseDown(MouseEvent e) {
				DirectoryDialog directoryDialog = new DirectoryDialog(getShell());
				directoryDialog.setFilterPath(outputFolder);
				String tmp = directoryDialog.open();
				if (tmp != null){
					outputPath.setText(tmp);
					outputFolder = tmp;
					StorePreferences.saveValue("SelenPage_outputFolder", outputFolder);
				}
			}

			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		createNewProject.addMouseListener(new MouseListener() {
			
			
			public void mouseUp(MouseEvent e) {
				genNewProject = createNewProject.getSelection();
				outputPath.setEnabled(!genNewProject);
				outputBrowse.setEnabled(!genNewProject);
			}
			
			
			public void mouseDown(MouseEvent e) {
				
			}
			
			
			public void mouseDoubleClick(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
		});
		
		outputPath.setEnabled(false);
		outputBrowse.setEnabled(false);
		WUtil.createSpace(composite);
		
		// check for validity of test case
		label = new Label(composite, SWT.NONE);
		label.setText("Generate valid test cases only");
		
		final Button validTestCaseBtn = new Button(composite, SWT.CHECK);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		validTestCaseBtn.setText("Yes");
		validTestCaseBtn.setLayoutData(layoutData);
		validTestCaseBtn.setSelection(true);
		
		validTestCaseBtn.addMouseListener(new MouseListener() {
			
			
			public void mouseUp(MouseEvent e) {
				onlyValidTestCase = validTestCaseBtn.getSelection();
			}
			
			
			public void mouseDown(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			
			public void mouseDoubleClick(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
		});

		WUtil.createSpace(composite);
		
		// Input domain
		label = new Label(composite, SWT.NONE);
		label.setText("Domain input specification");
		
		final Text domainInputText = new Text(composite, SWT.BORDER);
		domainInputText.setText(domainInputFile);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		domainInputText.setLayoutData(layoutData);
		
		final Button domainInputBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		domainInputBrowse.setText("  Browse...  ");
		domainInputBrowse.setLayoutData(layoutData);
		
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
					StorePreferences.saveValue("SelenPage_domainInputFile", domainInputFile);
				}
			}

			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		// Package name
		label = new Label(composite, SWT.NONE);
		label.setText("Package Name");
		
		final Text packageText = new Text(composite, SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		packageText.setText(packageName);
		packageText.setLayoutData(layoutData);
		packageText.addModifyListener(new ModifyListener() {
			
			
			public void modifyText(ModifyEvent e) {
				packageName = packageText.getText();
				StorePreferences.saveValue("SelenPage_packageName", packageName);
			}
		});

		// Class name
		label = new Label(composite, SWT.NONE);
		label.setText("Class Name");
		
		final Text classNameText = new Text(composite, SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		classNameText.setText(className);
		classNameText.setLayoutData(layoutData);
		classNameText.addModifyListener(new ModifyListener() {
			
			
			public void modifyText(ModifyEvent e) {
				className = classNameText.getText();
				StorePreferences.saveValue("SelenPage_className", className);
			}
		});

		// Selenium Driver
		label = new Label(composite, SWT.NONE);
		label.setText("Selenium WebDriver");
		
		final Combo driverSelect = new Combo(composite, SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		driverSelect.setLayoutData(layoutData);
		driverSelect.setItems(Constants.driverList);
		
		driverSelect.select(0);
		for (int i = 0; i < Constants.driverList.length; i++){
			if (Constants.driverList[i].equals(seleniumDriver)){
				driverSelect.select(i);
				break;
			}
		}
		
		driverSelect.addSelectionListener(new SelectionListener() {
			
			
			public void widgetSelected(SelectionEvent e) {
				seleniumDriver = driverSelect.getText();
				StorePreferences.saveValue("SelenPage_seleniumDriver", seleniumDriver);
				getContainer().updateButtons();
			}
			
			
			public void widgetDefaultSelected(SelectionEvent e) {
				seleniumDriver = driverSelect.getText();
				StorePreferences.saveValue("SelenPage_seleniumDriver", seleniumDriver);
			}
		});
		
		// Selenium Driver
		label = new Label(composite, SWT.NONE);
		label.setText("Target Web App URL");
		
		final Text webPageText = new Text(composite, SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		webPageText.setText(targetPage);
		webPageText.setLayoutData(layoutData);
		webPageText.addModifyListener(new ModifyListener() {
			
			
			public void modifyText(ModifyEvent e) {
				targetPage = webPageText.getText();
				StorePreferences.saveValue("SelenPage_targetPage", targetPage);
			}
		});
		
		setControl(composite);
	}

	public String getCteFile() {
		return cteFile;
	}

	public void setCteFile(String cteFile) {
		this.cteFile = cteFile;
	}

	public String getPackageName() {
		return packageName;
	}

	public void setPackageName(String packageName) {
		this.packageName = packageName;
	}

	public String getClassName() {
		return className;
	}

	public void setClassName(String className) {
		this.className = className;
	}

	public String getTargetPage() {
		return targetPage;
	}

	public void setTargetPage(String targetPage) {
		this.targetPage = targetPage;
	}

	public String getSeleniumDriver() {
		return seleniumDriver;
	}

	public void setSeleniumDriver(String seleniumDriver) {
		this.seleniumDriver = seleniumDriver;
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

	public boolean isGenNewProject() {
		return genNewProject;
	}

	public void setGenNewProject(boolean genNewProject) {
		this.genNewProject = genNewProject;
	}

	public boolean isOnlyValidTestCase() {
		return onlyValidTestCase;
	}

}
