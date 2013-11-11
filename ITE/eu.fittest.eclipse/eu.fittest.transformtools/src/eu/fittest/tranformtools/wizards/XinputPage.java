package eu.fittest.tranformtools.wizards;


import org.eclipse.jface.preference.IPreferenceStore;
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

import eu.fittest.tranformtools.Activator;

public class XinputPage extends WizardPage {
	private String fsmModel;
	private String domainInputFile;
	private String logFolder;
	
	IPreferenceStore refStore =  Activator.getDefault().getPreferenceStore();
	
	protected XinputPage(String pageName) {
		super(pageName);
		
		domainInputFile = refStore.getString("XinputPage_domainInputFile");
		logFolder =  refStore.getString("XinputPage_logFolder");
	}

	
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);
		
		Label label = new Label(composite, SWT.NONE);
		label.setText("FSM model: ");
  		final Text fsmModelText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		fsmModelText.setText(fsmModel);
		
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
				
				String tmp = WUtil.getSelection(getShell(), "FSM Model", "Select a FSM model");
				if (!tmp.equals("")){ 
					fsmModelText.setText(tmp);
					fsmModel = tmp;
				}
			}

			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		WUtil.createSpace(composite);
		// Second row, output folder
		
		label = new Label(composite, SWT.NONE);
		label.setText("Log folder (optional):");
		final Text outputPath = new Text(composite, SWT.BORDER | SWT.SINGLE);
		outputPath.setText(logFolder);
		
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
					logFolder = tmp;
					
					if (!troposModel.equals("") && !logFolder.equals("")){
						setPageComplete(true);
					}
				}
				*/
				
				String tmp = WUtil.getSelection(getShell(), "Log Folder", "Select a folder that contains FITTEST logs");
				if (!tmp.equals("")){ 
					outputPath.setText(tmp);
					logFolder = tmp;
					
					refStore.setValue("XinputPage_logFolder", logFolder);
					
					if (!fsmModel.equals("") && !logFolder.equals("")){
						setPageComplete(true);
					}
				}
			}

			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		WUtil.createSpace(composite);

		if (domainInputFile == null || domainInputFile == ""){
			domainInputFile = fsmModel.replace(".fsm", ".xml");
			refStore.setValue("XinputPage_domainInputFile", domainInputFile);
		}
		
		
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
		
//		domainInputBrowse.setEnabled(false);
//		domainInputText.setEnabled(false);
		
		domainInputText.addKeyListener(new KeyListener() {
			
			public void keyReleased(KeyEvent e) {
				domainInputFile = domainInputText.getText();
				refStore.setValue("XinputPage_domainInputFile", domainInputFile);
			}
			
			public void keyPressed(KeyEvent e) {
				// TODO Auto-generated method stub
				
			}
		});
		
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
					refStore.setValue("XinputPage_domainInputFile", tmp);
				}
			}

			public void mouseUp(MouseEvent e) {
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
		return logFolder;
	}

	public void setOutputFolder(String outputFolder) {
		this.logFolder = outputFolder;
	}

	
}
