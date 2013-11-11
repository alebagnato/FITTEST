package eu.fittest.tranformtools.wizards;

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

import eu.fittest.tranformtools.Activator;

public class BatchGenPage extends WizardPage {
	private String cteFolder;
	private String packageName;
	private String domainInputFile;
	private String outputFolder;

	private String targetPage;
	private String seleniumDriver;

	private boolean generateForWeb = true;

	private boolean onlyValidTestCase = true;

	protected BatchGenPage(String pageName) {
		super(pageName);

		packageName = Activator.getDefault().getPreferenceStore().getString("SelenPage_packageName");
		domainInputFile = Activator.getDefault().getPreferenceStore().getString("SelenPage_domainInputFile");

		targetPage = Activator.getDefault().getPreferenceStore().getString("SelenPage_targetPage");
		seleniumDriver = Activator.getDefault().getPreferenceStore().getString("SelenPage_seleniumDriver");

		outputFolder = Activator.getDefault().getPreferenceStore().getString("SelenPage_outputFolder");
	}

	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);

		// CTE Folder
		Label label = new Label(composite, SWT.NONE);
		label.setText("CTE Folder: ");
		GridData layoutData = new GridData();
		label.setLayoutData(layoutData);
		
		final Text cteFolderText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 1;
		cteFolderText.setLayoutData(layoutData);
		cteFolderText.setText(cteFolder);

		Button cteFolderBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		cteFolderBrowse.setText("  Browse..  ");
		cteFolderBrowse.setLayoutData(layoutData);

		cteFolderBrowse.addMouseListener(new MouseListener() {

			public void mouseDoubleClick(MouseEvent e) {
			}

			public void mouseDown(MouseEvent e) {

				String tmp = WUtil.getSelection(getShell(), "CTE Folder",
						"Select a folder that contains cte files");
				if (!tmp.equals("")) {
					cteFolderText.setText(tmp);
					cteFolder = tmp;
				}
			}

			public void mouseUp(MouseEvent e) {
			}

		});

		WUtil.createSpace(composite);

		// Input domain
		label = new Label(composite, SWT.NONE);
		label.setText("Domain input specification");

		final Text domainInputText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		domainInputText.setText(domainInputFile);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 1;
		domainInputText.setLayoutData(layoutData);

		final Button domainInputBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		domainInputBrowse.setText("  Browse...  ");
		domainInputBrowse.setLayoutData(layoutData);

		domainInputBrowse.addMouseListener(new MouseListener() {

			public void mouseDoubleClick(MouseEvent e) {
			}

			public void mouseDown(MouseEvent e) {
				FileDialog fileDialog = new FileDialog(getShell());
				fileDialog.setFilterExtensions(new String[] { "*.xml" });
				fileDialog.setFileName(domainInputText.getText());
				fileDialog.setFilterPath(domainInputFile);
				String tmp = fileDialog.open();
				if (tmp != null) {
					domainInputText.setText(tmp);
					domainInputFile = tmp;
					Activator.getDefault().getPreferenceStore().setValue("SelenPage_domainInputFile",
							domainInputFile);
				}
			}

			public void mouseUp(MouseEvent e) {
			}

		});

		WUtil.createSpace(composite);

		// Output folder
		label = new Label(composite, SWT.NONE);
		label.setText("Output folder:");

		final Text outputPath = new Text(composite, SWT.BORDER | SWT.SINGLE);
		outputPath.setText(outputFolder);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 1;
		outputPath.setLayoutData(layoutData);

		final Button outputBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		outputBrowse.setText("  Browse...  ");
		outputBrowse.setLayoutData(layoutData);

		outputBrowse.addMouseListener(new MouseListener() {

			public void mouseDoubleClick(MouseEvent e) {
			}

			public void mouseDown(MouseEvent e) {
				DirectoryDialog directoryDialog = new DirectoryDialog(
						getShell());
				directoryDialog.setFilterPath(outputFolder);
				String tmp = directoryDialog.open();
				if (tmp != null) {
					outputPath.setText(tmp);
					outputFolder = tmp;
					Activator.getDefault().getPreferenceStore().setValue("SelenPage_outputFolder",
							outputFolder);
				}
			}

			public void mouseUp(MouseEvent e) {
			}

		});

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

		// Package name
		label = new Label(composite, SWT.NONE);
		label.setText("Package Name");

		final Text packageText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		packageText.setText(packageName);
		packageText.setLayoutData(layoutData);
		packageText.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				packageName = packageText.getText();
				Activator.getDefault().getPreferenceStore().setValue("SelenPage_packageName", packageName);
			}
		});

		WUtil.createSpace(composite);

		// Selenium Driver
		label = new Label(composite, SWT.NONE);
		label.setText("Selenium WebDriver");

		final Combo driverSelect = new Combo(composite, SWT.BORDER | SWT.SINGLE);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		driverSelect.setLayoutData(layoutData);
		driverSelect.setItems(Constants.driverList);

		driverSelect.select(0);
		for (int i = 0; i < Constants.driverList.length; i++) {
			if (Constants.driverList[i].equals(seleniumDriver)) {
				driverSelect.select(i);
				break;
			}
		}

		driverSelect.addSelectionListener(new SelectionListener() {

			public void widgetSelected(SelectionEvent e) {
				seleniumDriver = driverSelect.getText();
				Activator.getDefault().getPreferenceStore().setValue("SelenPage_seleniumDriver",
						seleniumDriver);
				getContainer().updateButtons();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
				seleniumDriver = driverSelect.getText();
				Activator.getDefault().getPreferenceStore().setValue("SelenPage_seleniumDriver",
						seleniumDriver);
			}
		});

		

		// default
		driverSelect.setEnabled(generateForWeb);
		

		setControl(composite);
	}

	public String getPackageName() {
		return packageName;
	}

	public void setPackageName(String packageName) {
		this.packageName = packageName;
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

	public boolean isOnlyValidTestCase() {
		return onlyValidTestCase;
	}

	public String getCteFolder() {
		return cteFolder;
	}

	public void setCteFolder(String cteFolder) {
		this.cteFolder = cteFolder;
	}

	public boolean isGenerateForWeb() {
		return generateForWeb;
	}

	public void setGenerateForWeb(boolean generateForWeb) {
		this.generateForWeb = generateForWeb;
	}

	
}
