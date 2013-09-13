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

public class AndroidGenPage extends WizardPage {
	private String cteFile;
	private String packageName;
	private String className;
	private String domainInputFile;
	private String outputFolder;

	private String androidTargetPackage;
	private String androidTargetActivity;

	private boolean onlyValidTestCase = true;

	protected AndroidGenPage(String pageName) {
		super(pageName);

		packageName = StorePreferences.getSavedValue("AndroidPage_packageName");
		className = StorePreferences.getSavedValue("AndroidPage_className");
		
		domainInputFile = StorePreferences
				.getSavedValue("AndroidPage_domainInputFile");

		outputFolder = StorePreferences.getSavedValue("AndroidPage_outputFolder");

		androidTargetPackage = StorePreferences
				.getSavedValue("AndroidPage_androidTargetPackage");
		androidTargetActivity = StorePreferences
				.getSavedValue("AndroidPage_androidTargetActivity");

	}

	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);

		// CTE Folder
		Label label = new Label(composite, SWT.NONE);
		label.setText("CTE File: ");
		final Text cteFileText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		cteFileText.setText(cteFile);

		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		cteFileText.setLayoutData(layoutData);

		Button cteFileBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		cteFileBrowse.setText("  Browse..  ");
		cteFileBrowse.setLayoutData(layoutData);

		cteFileBrowse.addMouseListener(new MouseListener() {

			public void mouseDoubleClick(MouseEvent e) {
			}

			public void mouseDown(MouseEvent e) {

				String tmp = WUtil.getSelection(getShell(), "CTE File",
						"Select a CTE file");
				if (!tmp.equals("")) {
					cteFileText.setText(tmp);
					cteFile = tmp;
				}
			}

			public void mouseUp(MouseEvent e) {
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
					StorePreferences.saveValue("AndroidPage_domainInputFile",
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
					StorePreferences.saveValue("AndroidPage_outputFolder",
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

		final Text packageText = new Text(composite, SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		packageText.setText(packageName);
		packageText.setLayoutData(layoutData);
		packageText.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				packageName = packageText.getText();
				StorePreferences
						.saveValue("AndroidPage_packageName", packageName);
			}
		});

		WUtil.createSpace(composite);
		// class name
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
				StorePreferences
				.saveValue("AndroidPage_className", className);
			}
		});
		
		WUtil.createSpace(composite);

		// Robotium Android package
		label = new Label(composite, SWT.NONE);
		label.setText("Android Target Package");

		final Text targetPackage = new Text(composite, SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		targetPackage.setText(androidTargetPackage);
		targetPackage.setLayoutData(layoutData);
		targetPackage.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				androidTargetPackage = targetPackage.getText();
				StorePreferences.saveValue("AndroidPage_androidTargetPackage",
						androidTargetPackage);
			}
		});

		label = new Label(composite, SWT.NONE);
		label.setText("Android Target Activity");

		final Text targetActivity = new Text(composite, SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		targetActivity.setText(androidTargetActivity);
		targetActivity.setLayoutData(layoutData);
		targetActivity.addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				androidTargetActivity = targetActivity.getText();
				StorePreferences.saveValue("AndroidPage_androidTargetActivity",
						androidTargetActivity);
			}
		});
		
		
		setControl(composite);
	}

	public String getPackageName() {
		return packageName;
	}

	public void setPackageName(String packageName) {
		this.packageName = packageName;
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

	public String getAndroidTargetPackage() {
		return androidTargetPackage;
	}

	public void setAndroidTargetPackage(String androidTargetPackage) {
		this.androidTargetPackage = androidTargetPackage;
	}

	public String getAndroidTargetActivity() {
		return androidTargetActivity;
	}

	public void setAndroidTargetActivity(String androidTargetActivity) {
		this.androidTargetActivity = androidTargetActivity;
	}

	public String getCteFile() {
		return cteFile;
	}

	public void setCteFile(String cteFile) {
		this.cteFile = cteFile;
	}

	public void setOnlyValidTestCase(boolean onlyValidTestCase) {
		this.onlyValidTestCase = onlyValidTestCase;
	}

	public String getClassName() {
		return className;
	}

	public void setClassName(String className) {
		this.className = className;
	}
	
	
	
}
