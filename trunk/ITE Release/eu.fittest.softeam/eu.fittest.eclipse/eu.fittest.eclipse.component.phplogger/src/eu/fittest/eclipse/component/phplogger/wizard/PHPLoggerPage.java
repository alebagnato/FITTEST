package eu.fittest.eclipse.component.phplogger.wizard;

import java.util.ArrayList;
import java.util.Collection;

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

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;


public class PHPLoggerPage extends WizardPage  implements IFITTESTComponentWizardPage{
	private Button _enabled;	
	private boolean _enabledValue = true;
	private String _applicationFolder;
	
	public PHPLoggerPage() {
		super("PHP logger initialization parameters");
		setTitle("PHP logger initialization parameters");
	}
	
	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NULL); // Fixed by Cu to work on Linux
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		composite.setLayout(gridLayout);
		
		GridData layoutData = new GridData();
		
		_enabled = new Button(composite, SWT.CHECK);
		_enabled.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				_enabledValue = _enabled.getSelection();
				dialogChanged();
			}
		});
		
		layoutData.horizontalAlignment = SWT.RIGHT;
		_enabled.setLayoutData(layoutData);
		_enabled.setSelection(_enabledValue);
		
		layoutData = new GridData();
		Label label = new Label(composite, SWT.NONE);
		label.setText("Enable PHP logging");
		layoutData.horizontalAlignment = SWT.LEFT;
		label.setLayoutData(layoutData);
	    label = new Label(composite, SWT.NONE);
		label.setText("Folder of Web Application");
		label.setLayoutData(layoutData);

		final Text webAppField = new Text(composite, SWT.SINGLE | SWT.BORDER);
		webAppField.setEditable(true);
		
		if (_applicationFolder != null)
			webAppField.setText(_applicationFolder);
		
		webAppField.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				_applicationFolder = webAppField.getText();
				dialogChanged();
			}
		});
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		layoutData.horizontalSpan=2;
		webAppField.setLayoutData(layoutData);

		_enabled.setSelection(_enabledValue);
		setControl(composite);
		dialogChanged();
	}
	
	private void dialogChanged() {
		updateStatus(null);	
	}
	
	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}
	

	public Collection<Parameter> getInitializationParameters() {
		ArrayList<Parameter> parameters = new ArrayList<Parameter>();
		Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
		p.setName("application.folder");
		p.setValue(_applicationFolder);
		parameters.add(p);
		return parameters;
	}

	public boolean isEnabled() {
		return _enabledValue;
	}

	public void setApplicationFolder(String _applicationFolder) {
		this._applicationFolder = _applicationFolder;
	}
	
}
