/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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

public class BatchGenPage extends WizardPage {
	private String cteFolder;
	private String packageName;
	private String domainInputFile;
	private String outputFolder;

	private String targetPage;
	private String seleniumDriver;

	private String androidTargetPackage;
	private String androidTargetActivity;

	private boolean generateForWeb = true;
	private boolean generateForAndroid = false;

	private boolean onlyValidTestCase = true;

	protected BatchGenPage(String pageName) {
		super(pageName);

		packageName = StorePreferences.getSavedValue("SelenPage_packageName");
		domainInputFile = StorePreferences
				.getSavedValue("SelenPage_domainInputFile");

		targetPage = StorePreferences.getSavedValue("SelenPage_targetPage");
		seleniumDriver = StorePreferences
				.getSavedValue("SelenPage_seleniumDriver");

		outputFolder = StorePreferences.getSavedValue("SelenPage_outputFolder");

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
					StorePreferences.saveValue("SelenPage_domainInputFile",
							domainInputFile);
				}
			}

			public void mouseUp(MouseEvent e) {
			}

		});

		WUtil.createSpace(composite);
		
		// New project or output folder
		label = new Label(composite, SWT.NONE);
		label.setText("Target:");

		final Button selenium = new Button(composite, SWT.CHECK);

		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		selenium.setText("Selenium/Web");
		selenium.setLayoutData(layoutData);
		selenium.setSelection(generateForWeb);

		label = new Label(composite, SWT.NONE);
		label.setText("");

		final Button robotium = new Button(composite, SWT.CHECK);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		robotium.setText("Robotium/Android");
		robotium.setLayoutData(layoutData);
		robotium.setSelection(generateForAndroid);
		
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
					StorePreferences.saveValue("SelenPage_outputFolder",
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
				StorePreferences
						.saveValue("SelenPage_packageName", packageName);
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
				StorePreferences.saveValue("SelenPage_seleniumDriver",
						seleniumDriver);
				getContainer().updateButtons();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
				seleniumDriver = driverSelect.getText();
				StorePreferences.saveValue("SelenPage_seleniumDriver",
						seleniumDriver);
			}
		});

		// target web page
		label = new Label(composite, SWT.NONE);
		label.setText("Target Web Page URL");

		final Text webPageText = new Text(composite, SWT.BORDER | SWT.SINGLE);
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

		WUtil.createSpace(composite);

		// Robotium Android package
		label = new Label(composite, SWT.NONE);
		label.setText("Android Target Package");

		final Text targetPackage = new Text(composite, SWT.BORDER | SWT.SINGLE);
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
		
		

		selenium.addMouseListener(new MouseListener() {

			public void mouseUp(MouseEvent e) {
				generateForWeb = selenium.getSelection();
				generateForAndroid = !generateForWeb;
				robotium.setSelection(generateForAndroid);
				
				driverSelect.setEnabled(generateForWeb);
				webPageText.setEnabled(generateForWeb);
				targetPackage.setEnabled(generateForAndroid);
				targetActivity.setEnabled(generateForAndroid);
			}

			public void mouseDown(MouseEvent e) {

			}

			public void mouseDoubleClick(MouseEvent e) {
				// TODO Auto-generated method stub

			}
		});
		robotium.addMouseListener(new MouseListener() {

			public void mouseUp(MouseEvent e) {
				generateForAndroid = robotium.getSelection();
				generateForWeb = !generateForAndroid;
				selenium.setSelection(generateForWeb);
				
				driverSelect.setEnabled(generateForWeb);
				webPageText.setEnabled(generateForWeb);
				targetPackage.setEnabled(generateForAndroid);
				targetActivity.setEnabled(generateForAndroid);
			}

			public void mouseDown(MouseEvent e) {

			}

			public void mouseDoubleClick(MouseEvent e) {
				// TODO Auto-generated method stub

			}
		});

		// default
		driverSelect.setEnabled(generateForWeb);
		webPageText.setEnabled(generateForWeb);
		targetPackage.setEnabled(generateForAndroid);
		targetActivity.setEnabled(generateForAndroid);
		

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

	public boolean isGenerateForWeb() {
		return generateForWeb;
	}

	public void setGenerateForWeb(boolean generateForWeb) {
		this.generateForWeb = generateForWeb;
	}

	public boolean isGenerateForAndroid() {
		return generateForAndroid;
	}

	public void setGenerateForAndroid(boolean generateForAndroid) {
		this.generateForAndroid = generateForAndroid;
	}

	
}
