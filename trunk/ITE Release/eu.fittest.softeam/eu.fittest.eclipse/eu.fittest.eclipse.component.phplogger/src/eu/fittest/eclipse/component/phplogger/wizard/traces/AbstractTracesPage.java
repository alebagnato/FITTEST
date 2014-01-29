package eu.fittest.eclipse.component.phplogger.wizard.traces;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.fittest.eclipse.component.phplogger.Activator;


public class AbstractTracesPage extends WizardPage{
	private String _pathToInputFolder = "";
	private Text _pathToInputFolderField;
	
	private String _pathToAbstractSpec = "";
	private Text _pathToAbstractSpecField;
	
	private String _pathToOutput = "";
	private Text _pathToOutputField;
	
	private static final String EVENT_ABSTRACTION_SPEC = "event_abstraction_spec";
	private static final String OUTPUT_FOLDER = "output_folder";
	
	public String getPathToAbstractSpec() {
		return _pathToAbstractSpec;
	}
	
	public String getPathToOutput() {
		return _pathToOutput;
	}

	public void setPathToOutput(String pathToOutput) {
		_pathToOutput = pathToOutput;
	}
	
	public String getPathToInputFolder() {
		return _pathToInputFolder;
	}

	public void setPathToInputFolder(String pathToInputFolder) {
		_pathToInputFolder = pathToInputFolder;
	}
	
	public AbstractTracesPage() {
		super("Trace abstraction parameters");
		setTitle("Traced abstraction parameters");
		
		_pathToAbstractSpec = eu.fittest.eclipse.component.phplogger.Activator.getDefault().getPreferenceStore().getString(EVENT_ABSTRACTION_SPEC);
		_pathToOutput = eu.fittest.eclipse.component.phplogger.Activator.getDefault().getPreferenceStore().getString(OUTPUT_FOLDER);
	}
	
	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NULL);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 3;
		composite.setLayout(gridLayout);
		
		
		
		/**********************************************************************/
		Label label = new Label(composite, SWT.NONE);
		label.setText("Input folder:");
		GridData layoutData = new GridData();
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);
		
		_pathToInputFolderField = new Text(composite, SWT.SINGLE | SWT.BORDER);
		_pathToInputFolderField.setText(_pathToInputFolder);
		_pathToInputFolderField.setEditable(false);
		
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		layoutData.horizontalSpan=2;
		_pathToInputFolderField.setLayoutData(layoutData);
		
		
		/**********************************************************************/
		
		
		
		
		layoutData = new GridData();
		
		label = new Label(composite, SWT.NONE);
		label.setText("Event abstraction specification:");
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);
		
		_pathToAbstractSpecField = new Text(composite, SWT.SINGLE | SWT.BORDER);
		_pathToAbstractSpecField.setEditable(false);
		_pathToAbstractSpecField.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				_pathToAbstractSpec = _pathToAbstractSpecField.getText();
				eu.fittest.eclipse.component.phplogger.Activator.getDefault().getPreferenceStore().setValue(EVENT_ABSTRACTION_SPEC, _pathToAbstractSpec);
				dialogChanged();
			}
		});
		
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		_pathToAbstractSpecField.setLayoutData(layoutData);
		_pathToAbstractSpecField.setText(_pathToAbstractSpec);
		
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = false;
		layoutData.horizontalAlignment = GridData.FILL;
		
		Button browse = new Button(composite, SWT.PUSH);
		browse.setLayoutData(layoutData);
		browse.setText("Browse...");
		browse.addMouseListener(new MouseListener(){

			
			public void mouseDoubleClick(MouseEvent e) {
			}

			
			public void mouseDown(MouseEvent e) {
				FileDialog fileDialog = new FileDialog(getShell());
				fileDialog.setFilterExtensions(new String[]{"*.xml"});
				fileDialog.setFileName(_pathToAbstractSpec);
				fileDialog.setFilterPath(_pathToAbstractSpec);
				String tmp = fileDialog.open();
				if (tmp != null){
					_pathToAbstractSpecField.setText(tmp);
					_pathToAbstractSpec = tmp;
					Activator.getDefault().getPreferenceStore().setValue(EVENT_ABSTRACTION_SPEC, tmp);
					dialogChanged();
				}
			}

			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		/**********************************************************************/
		label = new Label(composite, SWT.NONE);
		label.setText("Output folder:");
		layoutData = new GridData();
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);
		
		_pathToOutputField = new Text(composite, SWT.SINGLE | SWT.BORDER);
		_pathToOutputField.setText(_pathToOutput);
		_pathToOutputField.setEditable(false);
		_pathToOutputField.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				_pathToOutput = _pathToOutputField.getText();
				eu.fittest.eclipse.component.phplogger.Activator.getDefault().getPreferenceStore().setValue(OUTPUT_FOLDER, _pathToOutput);
				dialogChanged();
			}
		});
		
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		_pathToOutputField.setLayoutData(layoutData);
		
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = false;
		layoutData.horizontalAlignment = GridData.FILL;
		
		browse = new Button(composite, SWT.PUSH);
		browse.setLayoutData(layoutData);
		browse.setText("Browse...");
		browse.addMouseListener(new MouseListener(){

			
			public void mouseDoubleClick(MouseEvent e) {
			}

			
			public void mouseDown(MouseEvent e) {
				DirectoryDialog fileDialog = new DirectoryDialog(getShell());
				fileDialog.setFilterPath(_pathToOutput);
				String tmp = fileDialog.open();
				if (tmp != null){
					_pathToOutputField.setText(tmp);
					_pathToOutput = tmp;
					eu.fittest.eclipse.component.phplogger.Activator.getDefault().getPreferenceStore().setValue(OUTPUT_FOLDER, _pathToOutput);
					dialogChanged();
				}
			}

			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		setControl(composite);
		dialogChanged();
	}
	
	private void dialogChanged() {
		if(_pathToAbstractSpec==null || _pathToAbstractSpec.length()==0){
			updateStatus("Select an event abstraction specification");
		}
		else {
			updateStatus(null);	
		}
	}
	
	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

}
