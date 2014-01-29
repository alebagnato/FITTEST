package eu.fittest.eclipse.oraclesuite.wizards;


import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class CheckDaikonPage extends WizardPage {


	private String oldLogFolder = "";

	private String newLogFolder = "";

	private String oracleFile = "";

	private String violationRecordFile = ""; 

	private String reportFile = ""; 


	protected CheckDaikonPage(String pageName) {
		super(pageName);

	}


	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);

		Label label = new Label(composite, SWT.NONE);
		label.setText("Old log folder: ");
		final Text fsmModelText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		fsmModelText.setText(oldLogFolder);

		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		fsmModelText.setLayoutData(layoutData);

		Button fsmBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		fsmBrowse.setText("  Browse..  ");
		fsmBrowse.setLayoutData(layoutData);

		fsmBrowse.addMouseListener(new MouseListener(){


			public void mouseDoubleClick(MouseEvent e) {
			}


			public void mouseDown(MouseEvent e) {

				String tmp = WUtil.getSelection(getShell(), "Old Log folder", "Select a log folder");
				if (!tmp.equals("")){ 
					fsmModelText.setText(tmp);
					oldLogFolder = tmp;
				}
			}


			public void mouseUp(MouseEvent e) {
			}

		});

		WUtil.createSpace(composite);

		// Second row, new log folder 

		label = new Label(composite, SWT.NONE);
		label.setText("New log folder :");
		final Text newLogFolderPath = new Text(composite, SWT.BORDER | SWT.SINGLE);
		newLogFolderPath.setText(newLogFolder);

		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		newLogFolderPath.setLayoutData(layoutData);

		Button outputBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		outputBrowse.setText("  Browse...  ");
		outputBrowse.setLayoutData(layoutData);

		outputBrowse.addMouseListener(new MouseListener(){


			public void mouseDoubleClick(MouseEvent e) {}


			public void mouseDown(MouseEvent e) {

				String tmp = WUtil.getSelection(getShell(), "Log Folder", "Select a folder that contains FITTEST logs");
				if (!tmp.equals("")){ 
					newLogFolderPath.setText(tmp);
					newLogFolder = tmp;
				}
			}


			public void mouseUp(MouseEvent e) {
			}

		});

		WUtil.createSpace(composite);


		// Third row, oracle File

		label = new Label(composite, SWT.NONE);
		label.setText("Oracle File");

		final Text oracleFileText = new Text(composite, SWT.BORDER);
		oracleFileText.setText(this.oracleFile);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		oracleFileText.setLayoutData(layoutData);


		oracleFileText.addKeyListener(new KeyListener() {

			public void keyReleased(KeyEvent e) {
				oracleFile = oracleFileText.getText();
			}

			public void keyPressed(KeyEvent e) {			}
		});

		oracleFileText.addMouseListener(new MouseListener(){


			public void mouseDoubleClick(MouseEvent e) {
			}


			public void mouseDown(MouseEvent e) {

				FileDialog fileDialog = new FileDialog(getShell());
				fileDialog.setFilterExtensions(new String[]{"*.xml"});
				fileDialog.setFileName(oracleFileText.getText());
				fileDialog.setFilterPath(oracleFile);
				String tmp = fileDialog.open();
				if (tmp != null){
					oracleFileText.setText(tmp);
					oracleFile = tmp;
				}
			}

			public void mouseUp(MouseEvent e) {
			}

		});

		WUtil.createSpace(composite);

		// fourth row, violation File

		label = new Label(composite, SWT.NONE);
		label.setText("Violation File");

		final Text violationFileText = new Text(composite, SWT.BORDER);
		violationFileText.setText(this.violationRecordFile);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		violationFileText.setLayoutData(layoutData);


		violationFileText.addKeyListener(new KeyListener() {

			public void keyReleased(KeyEvent e) {
				violationRecordFile = violationFileText.getText();
			}

			public void keyPressed(KeyEvent e) {			}
		});


		violationFileText.addMouseListener(new MouseListener(){


			public void mouseDoubleClick(MouseEvent e) {
			}


			public void mouseDown(MouseEvent e) {

				FileDialog fileDialog = new FileDialog(getShell());
				fileDialog.setFilterExtensions(new String[]{"*.xml"});
				fileDialog.setFileName(violationFileText.getText());
				String tmp = fileDialog.open();
				if (tmp != null){
					violationFileText.setText(tmp);
					violationRecordFile = tmp;
				}
			}

			public void mouseUp(MouseEvent e) {
			}

		});
		WUtil.createSpace(composite);

		// fith row, report File

		label = new Label(composite, SWT.NONE);
		label.setText("Report File : ");

		final Text reportFileText = new Text(composite, SWT.BORDER);
		reportFileText.setText(this.reportFile);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		reportFileText.setLayoutData(layoutData);


		reportFileText.addKeyListener(new KeyListener() {

			public void keyReleased(KeyEvent e) {
				reportFile = reportFileText.getText();
			}

			public void keyPressed(KeyEvent e) {			}
		});


		reportFileText.addMouseListener(new MouseListener(){


			public void mouseDoubleClick(MouseEvent e) {
			}


			public void mouseDown(MouseEvent e) {

				FileDialog fileDialog = new FileDialog(getShell());
				fileDialog.setFilterExtensions(new String[]{"*.txt"});
				fileDialog.setFileName(violationFileText.getText());
				String tmp = fileDialog.open();
				if (tmp != null){
					violationFileText.setText(tmp);
					violationRecordFile = tmp;
				}
			}

			public void mouseUp(MouseEvent e) {
			}

		});
		WUtil.createSpace(composite);


		setControl(composite);
		return;
	}


	public String getOldLogFolder() {
		return this.oldLogFolder;
	}

	public void setOldLogFolder(String oldLogFolder) {
		this.oldLogFolder = oldLogFolder;
	}

	public String getNewLogFolder() {
		return this.newLogFolder;
	}

	public void setNewLogFolder(String newLogFolder) {
		this.newLogFolder = newLogFolder;
	}

	public String getOracleFile() {
		return this.oracleFile;
	}

	public void setOracleFile(String oracleFile) {
		this.oracleFile = oracleFile;
	}

	public String getViolationRecordFile() {
		return this.violationRecordFile;
	}

	public void setViolationRecordFile(String violationRecordFile) {
		this.violationRecordFile = violationRecordFile;
	}
	
	public String getReportFile() {
		return this.reportFile;
	}

	public void setReportFile(String reportFile) {
		this.reportFile = reportFile;
	}


}
