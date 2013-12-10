package eu.fittest.eclipse.asli.wizard;


import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.fittest.eclipse.asli.Activator;

public class AsliPage extends WizardPage {
	private String _specification = "";
	private String _inputSWFFile = "";
	private String _outputSWFFile = "";
	
	private Text _specificationField;
	private Text _inputSWFField;
	private Text _outputSWFField;

	private FileDialog _fileDialog = null; 
	
	protected AsliPage() {
		super("Instrument Flash application with log-points");
		setTitle("Instrument Flash application with log-points");
		ImageDescriptor imgDes = Activator.getImageDescriptor("resource/icons/flash1.png");
		setImageDescriptor(imgDes);
		
		_inputSWFFile = null;
		_outputSWFFile = null;
	}

	
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NULL);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 3;
		composite.setLayout(gridLayout);
		
		GridData layoutData = new GridData();
		
		Label label = new Label(composite, SWT.NONE);
		label.setText("Log point specification:");
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);
		
		_specificationField = new Text(composite, SWT.SINGLE | SWT.BORDER);
		_specificationField.setText(_specification);
		_specificationField.setEditable(false);
		
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		layoutData.horizontalSpan = 2;
		_specificationField.setLayoutData(layoutData);
		
		///////////////////////////////////////////////////////////////////:
		layoutData = new GridData();
		
		label = new Label(composite, SWT.NONE);
		label.setText("Input Flash application:");
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);
		
		 _inputSWFField = new Text(composite, SWT.SINGLE | SWT.BORDER);
		 _inputSWFField.addModifyListener(new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent e) {
				_inputSWFFile = _inputSWFField.getText();
				
			}
		});
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		_inputSWFField.setLayoutData(layoutData);
		_inputSWFField.setEditable(false);
		
		layoutData = new GridData();
		Button browse = new Button(composite, SWT.NONE);
		browse.setText("Browse...");
		
		_fileDialog = new FileDialog(getShell());
		_fileDialog.setFilterExtensions(new String[]{"*.swf"});
		
		browse.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {				
				String tmp = _fileDialog.open();
				if (tmp != null){
					_inputSWFField.setText(tmp);
					dialogChanged();
				}
			}
		});
		
		browse.setLayoutData(layoutData);
		
		/////////////////////////////////////////////////////////////////////
		
		layoutData = new GridData();
		
		label = new Label(composite, SWT.NONE);
		label.setText("Output Flash application:");
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);
		
		 _outputSWFField = new Text(composite, SWT.SINGLE | SWT.BORDER);
		 _outputSWFField.addModifyListener(new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent e) {
				_outputSWFFile = _outputSWFField.getText();
				dialogChanged();
			}
		});
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		_outputSWFField.setLayoutData(layoutData);	
		_outputSWFField.setEditable(false);
		layoutData = new GridData();
		Button browseOut = new Button(composite, SWT.NONE);
		browseOut.setText("Browse...");
		
		if(_fileDialog==null){
			_fileDialog = new FileDialog(getShell());
			_fileDialog.setFilterExtensions(new String[]{"*.swf"});
		}
		
		browseOut.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {				
				String tmp = _fileDialog.open();
				if (tmp != null){
					_outputSWFField.setText(tmp);
					dialogChanged();
				}
			}
		});
		
		browseOut.setLayoutData(layoutData);
		
		dialogChanged();
		setControl(composite);
	}
	
	private void dialogChanged() {
		if(_inputSWFFile == null ||_inputSWFFile.length() == 0){
			updateStatus("Input Flash application can't be empty");
		}
		else if(_outputSWFFile == null || _outputSWFFile.length() == 0){
			updateStatus("Output SWF file can't be empty");
		}
		else {
			updateStatus(null);
		}
	}
	
	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

	public String getOutputFile() {
		return _outputSWFFile;
	}
	
	public String getInputFile() {
		return _inputSWFFile;
	}
	
	public void setSpecification(String specification){
		_specification = specification;
	}
	
}
