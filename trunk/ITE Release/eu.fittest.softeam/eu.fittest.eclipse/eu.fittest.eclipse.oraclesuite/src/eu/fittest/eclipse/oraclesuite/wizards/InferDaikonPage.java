package eu.fittest.eclipse.oraclesuite.wizards;


import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;
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

import eu.fittest.eclipse.oraclesuite.Activator;
import eu.fittest.eclipse.oraclesuite.utils.Utils;

public class InferDaikonPage extends WizardPage {
	
	private String logDir = "";
	
	private String gHRCTopts = "";
	
	private String oracleFile = "";
	
	private String reportFile = "";
	
	private String eventsToInclude = "" ;
	
	private String fieldsToInclude = "";
	
	
	IPreferenceStore refStore =  Activator.getDefault().getPreferenceStore();
	
	protected InferDaikonPage(String pageName) {
		super(pageName);
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);
		
		Label label = new Label(composite, SWT.NONE);
		label.setText("Log folder: ");
  		final Text logFolderText = new Text(composite, SWT.BORDER | SWT.SINGLE);
  		logFolderText.setText(this.logDir);
		
		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		logFolderText.setLayoutData(layoutData);
		
		Button fsmBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		fsmBrowse.setText("  Browse..  ");
		fsmBrowse.setLayoutData(layoutData);
		
		fsmBrowse.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {
				
				String tmp = WUtil.getSelection(getShell(), "Log folder", "Select a log Folder");
				if (!tmp.equals("")){ 
					logFolderText.setText(tmp);
					logDir = tmp;
				}
			}

			@Override
			public void mouseUp(MouseEvent e) {}
			
		});
		
		WUtil.createSpace(composite);
		
		// Second row, GHRCTopts
		
		label = new Label(composite, SWT.NONE);
		label.setText("GHCRTopts : ");
		final Text ghrctOptsText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		ghrctOptsText.setText(this.gHRCTopts);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		ghrctOptsText.setLayoutData(layoutData);
		
		ghrctOptsText.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {			
				gHRCTopts = ghrctOptsText.getText();
			}

			@Override
			public void mouseUp(MouseEvent e) {}
			
		});
		
		WUtil.createSpace(composite);
		
		// third row, Oracle File
		
		label = new Label(composite, SWT.NONE);
		label.setText("Oracle File name : ");
		final Text oracleFileText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		oracleFileText.setText(this.oracleFile);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		oracleFileText.setLayoutData(layoutData);
		oracleFileText.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {			
				InferDaikonPage.this.oracleFile = oracleFileText.getText();
			}

			@Override
			public void mouseUp(MouseEvent e) {}
			
		});
		
		WUtil.createSpace(composite);
		
		
		// forth row, Report File
		
		label = new Label(composite, SWT.NONE);
		label.setText("Report File name : ");
		final Text reportFileText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		reportFileText.setText(this.reportFile);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		reportFileText.setLayoutData(layoutData);
		reportFileText.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {			
				InferDaikonPage.this.oracleFile = reportFileText.getText();
			}

			@Override
			public void mouseUp(MouseEvent e) {}
			
		});
		
		
		WUtil.createSpace(composite);
		
		
		// fifth row, events to include File
		
		label = new Label(composite, SWT.NONE);
		label.setText("Events to include : ");
		final Text eventsToIncludeText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		eventsToIncludeText.setText(this.eventsToInclude);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		eventsToIncludeText.setLayoutData(layoutData);
		eventsToIncludeText.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {			
				InferDaikonPage.this.eventsToInclude = eventsToIncludeText.getText();
			}

			@Override
			public void mouseUp(MouseEvent e) {}
			
		});
		
		WUtil.createSpace(composite);
	
		
		// sixth row, fields to include File
		
		label = new Label(composite, SWT.NONE);
		label.setText("Fields to include : ");
		final Text fieldsToIncludeText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		fieldsToIncludeText.setText(this.fieldsToInclude);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		fieldsToIncludeText.setLayoutData(layoutData);
		fieldsToIncludeText.addMouseListener(new MouseListener(){

			@Override
			public void mouseDoubleClick(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {			
				InferDaikonPage.this.fieldsToInclude = fieldsToIncludeText.getText();
			}

			@Override
			public void mouseUp(MouseEvent e) {}
			
		});
			
		
		WUtil.createSpace(composite);
		
		
		setControl(composite);
		return;
	}
	

	public String getLogDir() {
		return this.logDir;
	}

	public void setLogDir(String logDir) {
		this.logDir = logDir;
	}

	public String getGHRCopts() {
		return this.gHRCTopts;
	}

	public void setGHRCTopts(String gHRCTopts) {
		this.gHRCTopts = gHRCTopts;
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

	
	public String getEventsToInclude(){
		return this.eventsToInclude;
	}
	
	public void setEventsToInclude(String eventsToinclude){	
		this.eventsToInclude = eventsToinclude;
	}
	
	
	public List<String> getFieldsToInclude(){
		String[] fields = this.fieldsToInclude.split(Utils.getFieldDelimiter());
		return Arrays.asList(fields);
	}
	
	public void setFieldsToInclude(String fieldsToinclude){
		this.fieldsToInclude = fieldsToinclude;
	}
	
}
