package eu.fittest.eclipse.gui.wizards.project;

import java.util.ArrayList;
import java.util.List;

import javax.naming.InvalidNameException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.fittest.eclipse.gui.utils.Validation;
import eu.fittest.eclipse.gui.wizards.FITTESTWizardMessages;
import eu.fittest.project.config.SUTTechnologyType;

public class FITTESTProjectWizardPageOne extends WizardPage {
	private static final String PAGE_NAME = FITTESTWizardMessages.FITTESTProjectWizardPageOne_PageName;
	
	private Text projectName = null;
	private Combo projectTypeSelect = null;
	private Text projectBaseURL = null;
	private Text projectEntryPage = null;
	private Text projectServerFolder = null;
	
	protected FITTESTProjectWizardPageOne() {
		super(PAGE_NAME);
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 3;
		composite.setLayout(gridLayout);
		
		GridData layoutData = new GridData();
		
		Label label = new Label(composite, SWT.NONE);
		label.setText("Test project name:");
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);
		
		this.projectName = new Text(composite, SWT.SINGLE | SWT.BORDER);
		this.projectName.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				dialogChanged();
			}
		});
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		this.projectName.setLayoutData(layoutData);
		
		// Project Type
		label = new Label(composite, SWT.NONE);
		label.setText("SUT type:");
		
		projectTypeSelect = new Combo(composite, SWT.BORDER);
		layoutData = new GridData(GridData.FILL);
		projectTypeSelect.setLayoutData(layoutData);
		List<String> visitorNames = new ArrayList<String>();
		//for (SUTTechnologyType sutType : SUTTechnologyType.VALUES){
		for (SUTTechnologyType sutType : SUTTechnologyType.values()) {
			//visitorNames.add(sutType.getName());
			visitorNames.add(sutType.toString());
		}
		
		projectTypeSelect.setItems((String[]) visitorNames.toArray(new String[visitorNames.size()]));
		projectTypeSelect.select(0);
		
		label = new Label(composite, SWT.NONE);
		label.setText("");
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		label.setLayoutData(layoutData);

		// space
		label = new Label(composite, SWT.NONE);
		GridData dg = new GridData();
		dg.horizontalSpan = 3;
		dg.heightHint = 3;
		label.setLayoutData(dg);
		
		
		// SUT Server info
		
		label = new Label(composite, SWT.NONE);
		label.setText("Base URL:");
		projectBaseURL = new  Text(composite, SWT.SINGLE | SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		projectBaseURL.setLayoutData(layoutData);
		
		label = new Label(composite, SWT.NONE);
		label.setText("Entry page:");
		projectEntryPage = new  Text(composite, SWT.SINGLE | SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		projectEntryPage.setLayoutData(layoutData);

		// Server Folder
		label = new Label(composite, SWT.NONE);
		label.setText("Server folder (PHP):");
		projectServerFolder = new  Text(composite, SWT.SINGLE | SWT.BORDER);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		projectServerFolder.setLayoutData(layoutData);
		
		Button traceFolderBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 120;
		traceFolderBrowse.setText(" Browse...  ");
		traceFolderBrowse.setLayoutData(layoutData);
		
		
		traceFolderBrowse.addMouseListener(new MouseListener(){
			public void mouseDoubleClick(MouseEvent e) {
			}
			
			public void mouseDown(MouseEvent e) {
				DirectoryDialog directoryDialog = new DirectoryDialog(getShell());
				String tmp = directoryDialog.open();
				if (tmp != null){
					projectServerFolder.setText(tmp);
				}
			}
			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		this.projectName.setFocus();
		
		dialogChanged();
		setControl(composite);
	}
	
	

	public String getProjectType() {
		return projectTypeSelect.getText();
	}

	public String getProjectBaseURL() {
		return projectBaseURL.getText();
	}

	public String getProjectEntryPage() {
		return projectEntryPage.getText();
	}

	public String getProjectServerFolder() {
		return projectServerFolder.getText();
	}

	public String getProjectName() {
		return this.projectName.getText();
	}
	
	private void dialogChanged() {
		if(this.projectName.getText()==null || this.projectName.getText().length()==0){
			updateStatus("Project name for a SUT name must not be empty");
		}
		else if(alreadyExist(this.projectName.getText())){
			updateStatus("Project name already exists");
		}
		else {
			try{
				Validation.containsBadChar(this.projectName.getText());
				updateStatus(null);
			}catch (InvalidNameException e) {
				updateStatus("Name can contain only " + e.getExplanation());	
			}
		}
	}

	private boolean alreadyExist(String text) {
		String lowerText = text.toLowerCase();
		IProject [] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
		boolean found = false;
		int i = 0;
		while((!found) && (i < projects.length)){
			found = lowerText.equals(projects[i].getName().toLowerCase());
			i++;
		}
		
		return found;
	}

	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}
}
