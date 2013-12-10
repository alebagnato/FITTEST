package eu.fittest.eclipse.component.contest.wizard;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.InvalidNameException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;
import org.eclipse.ui.model.BaseWorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.component.contest.Activator;
import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.utils.Validation;


public class ConTestPage extends WizardPage  implements IFITTESTComponentWizardPage{
	private Text _targetClasses;
	protected Button _enabled;
	
	private String _targetClassesValue=null;
	private boolean _enabledValue = false;
	
	private Button _methodCoverage;
	private boolean _enabledMethodCoverage = false;
	
	private Button _basicBlockCoverage;
	private boolean _enabledBasicBlockCoverage = false;
	
	private Button _syncCoverage;
	private boolean _enabledSyncCoverage = false;
	
	private Button _browseWorkspace;
	private String _pathToKingProperties = "";
	private Text _pathToKPField;
	
	public ConTestPage() {
		super("ConTest initialization parameters");
		setTitle("ConTest initialization parameters");
		_targetClassesValue = Activator.getDefault().getPreferenceStore().getString("targetClasses");
	}
	
	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.EMBEDDED);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 3;
		composite.setLayout(gridLayout);
		
		GridData layoutData = new GridData();
		
		_enabled = new Button(composite, SWT.CHECK);
		_enabled.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				_enabledValue = _enabled.getSelection();
				enableContent();
				dialogChanged();
			}
		});
		
		layoutData.horizontalAlignment = SWT.RIGHT;
		_enabled.setLayoutData(layoutData);
		
		layoutData = new GridData();
		layoutData.horizontalSpan=2;
		Label label = new Label(composite, SWT.NONE);
		label.setText("Enable ConTest component");
		layoutData.horizontalAlignment = SWT.LEFT;
		label.setLayoutData(layoutData);
		
		layoutData = new GridData();
		label = new Label(composite, SWT.NONE);
		label.setText("Target classes:");
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);
		
		_targetClasses = new Text(composite, SWT.SINGLE | SWT.BORDER);
		_targetClasses.setText(_targetClassesValue);
		_targetClasses.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				_targetClassesValue = _targetClasses.getText();
				Activator.getDefault().getPreferenceStore().setValue("targetClasses", _targetClassesValue);
				dialogChanged();
			}
		});
		
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		layoutData.horizontalSpan=2;
		_targetClasses.setLayoutData(layoutData);

		
		/******************************************************************/
		layoutData = new GridData();
		label = new Label(composite, SWT.NONE);
		label.setText("Start from King Properties file:");
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);
		
		_pathToKPField = new Text(composite, SWT.SINGLE | SWT.BORDER);
		_pathToKPField.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				_pathToKingProperties = _pathToKPField.getText();
				dialogChanged();
			}
		});
		
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		_pathToKPField.setLayoutData(layoutData);
		
		_browseWorkspace = new Button(composite, SWT.PUSH);
		_browseWorkspace.setText("Browse...");
		_browseWorkspace.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ILabelProvider lp= new WorkbenchLabelProvider();
				ITreeContentProvider cp= new BaseWorkbenchContentProvider(); 
				
				ElementTreeSelectionDialog dialog = new ElementTreeSelectionDialog(getShell(),lp, cp);
				dialog.addFilter(new KingPropertiesFilter());
				dialog.setAllowMultiple(false);
				dialog.setTitle(""); 
				dialog.setMessage("Select a King Properties file in the workspace");
				dialog.setInput(ResourcesPlugin.getWorkspace().getRoot());
				
				if (dialog.open() == ElementTreeSelectionDialog.OK) {
					Object[] elements= dialog.getResult();
					if (elements.length == 1) {
						_pathToKPField.setText(((IResource)elements[0]).getLocation().toOSString());
						_pathToKingProperties = _pathToKPField.getText();
					}
				}
			}
		});
		
		/******************************************************************/
		layoutData = new GridData();
		_methodCoverage = new Button(composite, SWT.CHECK);
		_methodCoverage.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				_enabledMethodCoverage = _methodCoverage.getSelection();
			}
		});
		
		layoutData.horizontalAlignment = SWT.RIGHT;
		_methodCoverage.setLayoutData(layoutData);
		
		layoutData = new GridData();
		label = new Label(composite, SWT.NONE);
		label.setText("Enable method coverage");
		layoutData.horizontalAlignment = SWT.LEFT;
		layoutData.horizontalSpan=2;
		label.setLayoutData(layoutData);
		/******************************************************************/
		
		layoutData = new GridData();
		_basicBlockCoverage = new Button(composite, SWT.CHECK);
		_basicBlockCoverage.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				_enabledBasicBlockCoverage = _basicBlockCoverage.getSelection();
			}
		});
		
		layoutData.horizontalAlignment = SWT.RIGHT;
		_basicBlockCoverage.setLayoutData(layoutData);
		
		layoutData = new GridData();
		label = new Label(composite, SWT.NONE);
		label.setText("Enable basic block coverage");
		layoutData.horizontalSpan=2;
		layoutData.horizontalAlignment = SWT.LEFT;
		label.setLayoutData(layoutData);
		/******************************************************************/
		layoutData = new GridData();
		_syncCoverage = new Button(composite, SWT.CHECK);
		_syncCoverage.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				_enabledSyncCoverage = _syncCoverage.getSelection();
			}
		});
		
		layoutData.horizontalAlignment = SWT.RIGHT;
		_syncCoverage.setLayoutData(layoutData);
		
		layoutData = new GridData();
		label = new Label(composite, SWT.NONE);
		label.setText("Enable concurrency coverage");
		layoutData.horizontalSpan=2;
		layoutData.horizontalAlignment = SWT.LEFT;
		label.setLayoutData(layoutData);
		/******************************************************************/
		
		
	
		
		
		_enabled.setSelection(_enabledValue);
		setControl(composite);
		enableContent();
		dialogChanged();
	}

	protected void enableContent(){
		_targetClasses.setEnabled(_enabledValue);
		_methodCoverage.setEnabled(_enabledValue);
		_basicBlockCoverage.setEnabled(_enabledValue);
		_syncCoverage.setEnabled(_enabledValue);
		_browseWorkspace.setEnabled(_enabledValue);
		_pathToKPField.setEnabled(_enabledValue);
	}
	
	protected String dialogChanged() {
		String message = null;
		if(_enabledValue){
			if((_targetClassesValue==null || _targetClassesValue.equals("")) && _pathToKingProperties.length()==0){
				message = "Target classes can be empty only if a King Properties file is provided. Separate values with a comma";
			}
			else {
				try {
					Validation.containsBadChar(_targetClassesValue);
				} catch (InvalidNameException e) {
					message = "Target classes can only contain "+e.getMessage();
				}
			}
		}
		updateStatus(message);
		return message;
	}
	
	protected void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);;
	}
	

	public Collection<Parameter> getInitializationParameters() {
		ArrayList<Parameter> parameters = new ArrayList<Parameter>();

		parameters.add(createParameter("targetClasses", _targetClassesValue));
		parameters.add(createParameter("methodCoverage", Boolean.toString(_enabledMethodCoverage)));		
		parameters.add(createParameter("basicBlockCoverage", Boolean.toString(_enabledBasicBlockCoverage)));		
		parameters.add(createParameter("synchronizationCoverage", Boolean.toString(_enabledSyncCoverage)));		
		
		if(_pathToKingProperties!=null && _pathToKingProperties.length()>0){
			File KPfile= new File(_pathToKingProperties);
			if(KPfile.exists()){
				Properties prFile = new Properties();
				FileInputStream fos;
				try {
					fos = new FileInputStream(KPfile);
					prFile.load(fos);
					for(Object key : prFile.keySet()){
						parameters.add(createParameter(key.toString(), prFile.get(key).toString()));
					}
					
				} catch (FileNotFoundException e) {
					Logger.getAnonymousLogger().log(Level.SEVERE,e.getMessage());
				} catch (IOException e) {
					Logger.getAnonymousLogger().log(Level.SEVERE,e.getMessage());
				}
			}
		}
		
		return parameters;
	}
	
	private Parameter createParameter(String name, String value){
		Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
		p.setName(name);
		p.setValue(value);
		return p;
	}

	public boolean isEnabled() {
		return _enabledValue;
	}
	
	private class KingPropertiesFilter extends ViewerFilter{

		@Override
		public boolean select(Viewer viewer, Object parentElement, Object element) {
			return (element instanceof IProject ||
					(parentElement instanceof IProject && element instanceof IFolder && ((IFolder)element).getName().equals(IFITTESTFolderConstants.TESTING_SESSIONS)) ||
					(!(parentElement instanceof IProject) && element instanceof IFolder) ||
					(element instanceof IFile && ((IFile)element).getName().equals("KingProperties")));
		}

		
	}

}
