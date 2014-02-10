package eu.fittest.eclipse.oraclesuite.wizards;


import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.fittest.eclipse.oraclesuite.utils.Utils;

public class InferlloPage extends WizardPage {

	private String logFolder = "";

	private String ghrctopts = "";	

	private String oracleFile = "";

	private String reportFile = "";

	private String functionsToInclude = "";

	private String otherLlloOption = ""; 


	protected InferlloPage(String pageName) {
		super(pageName);	
	}


	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);

		Label label = new Label(composite, SWT.NONE);
		label.setText("Log folder ");
		final Text logFolderText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		logFolderText.setText(logFolder);

		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		logFolderText.setLayoutData(layoutData);

		Button fsmBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		fsmBrowse.setText("  Browse..  ");
		fsmBrowse.setLayoutData(layoutData);

		fsmBrowse.addMouseListener(new MouseListener(){


			public void mouseDoubleClick(MouseEvent e) {}


			public void mouseDown(MouseEvent e) {

				String tmp = WUtil.getSelection(getShell(), "log Folder", "Select a log Folder");
				if (!tmp.equals("")){ 
					logFolderText.setText(tmp);
					logFolder = tmp;
				}
			}


			public void mouseUp(MouseEvent e) {
			}

		});

		WUtil.createSpace(composite);

		// Second row, GHRCTopts

		label = new Label(composite, SWT.NONE);
		label.setText("GHRCT options :");
		final Text gHRCTOptionsText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		gHRCTOptionsText.setText(this.ghrctopts);

		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		gHRCTOptionsText.setLayoutData(layoutData);
		gHRCTOptionsText.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {			
				ghrctopts = gHRCTOptionsText.getText();
			}

			@Override
			public void mouseUp(MouseEvent e) {}

		});

		WUtil.createSpace(composite);

		// third row, Oracle File

		label = new Label(composite, SWT.NONE);
		label.setText("Oracle File:");
		final Text oracleFileText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		oracleFileText.setText(this.oracleFile);

		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		oracleFileText.setLayoutData(layoutData);
		oracleFileText.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {			
				oracleFile = oracleFileText.getText();
			}

			@Override
			public void mouseUp(MouseEvent e) {}

		});

		WUtil.createSpace(composite);


		// Forth row, Report File

		label = new Label(composite, SWT.NONE);
		label.setText("Report File:");
		final Text reportFileText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		reportFileText.setText(this.reportFile);

		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		reportFileText.setLayoutData(layoutData);
		reportFileText.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {			
				reportFile = reportFileText.getText();
			}

			@Override
			public void mouseUp(MouseEvent e) {}

		});

		WUtil.createSpace(composite);

		// Fith row, Function to include

		label = new Label(composite, SWT.NONE);
		label.setText("Included Functions:");
		final Text functionsToIncludeText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		functionsToIncludeText.setText(this.functionsToInclude);

		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		functionsToIncludeText.setLayoutData(layoutData);
		functionsToIncludeText.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {			
				functionsToInclude = functionsToIncludeText.getText();
			}

			@Override
			public void mouseUp(MouseEvent e) {}

		});

		WUtil.createSpace(composite);

		// Sixth row, LLO options

		label = new Label(composite, SWT.NONE);
		label.setText("Other Options :");
		final Text lloOptionsText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		lloOptionsText.setText(this.otherLlloOption);

		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		lloOptionsText.setLayoutData(layoutData);
		lloOptionsText.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {			
				otherLlloOption = lloOptionsText.getText();
			}

			@Override
			public void mouseUp(MouseEvent e) {}

		});

		WUtil.createSpace(composite);


		setControl(composite);
		return;
	}


	public String getLogFolder() {
		return this.logFolder;
	}

	public void setLogFolder(String logFolder) {
		this.logFolder = logFolder;
	}

	public String getGHRCTopts() {
		return this.ghrctopts;
	}

	public void setGHRCTopts(String ghrctopts) {
		this.ghrctopts = ghrctopts;
	}

	public String getOracleFile() {
		return this.oracleFile;
	}

	public void setOracleFile(String oracleFile) {
		this.oracleFile = oracleFile;
	}

	public String getReportFile() {
		return this.reportFile;
	}

	public void setReportFile(String reportFile) {
		this.reportFile = reportFile;
	}

	public List<String> getFunctionToInclude() {
		String[] functions = this.functionsToInclude.split(Utils.getFieldDelimiter());
		return Arrays.asList(functions); 
	}

	public void setFunctionToInclude(String functionsToInclude) {
		this.functionsToInclude = functionsToInclude;
	}

	public String getOtherLLOOption() {
		return this.otherLlloOption;
	}

	public void setOtherLLOOption(String otherLlloOption) {
		this.otherLlloOption = otherLlloOption;
	}


}
