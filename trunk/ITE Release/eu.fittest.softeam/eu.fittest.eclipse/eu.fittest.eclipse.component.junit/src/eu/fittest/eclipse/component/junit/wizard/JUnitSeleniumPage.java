package eu.fittest.eclipse.component.junit.wizard;

import java.io.File;
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
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.component.junit.Activator;


public class JUnitSeleniumPage extends WizardPage  implements IFITTESTComponentWizardPage{
	private Text _pathToProfileField;
	private String _pathToProfile=null;
	
	private Button _trustSSL;
	private boolean _enabledtrustSSL = false;

	
	public JUnitSeleniumPage() {
		super("JUnit Selenium initialization parameters");
		setTitle("JUnit Selenium initialization parameters");
		_pathToProfile = Activator.getDefault().getPreferenceStore().getString(JUnitSeleniumPage.class.getSimpleName()+".pathToProfile");
		_enabledtrustSSL = Activator.getDefault().getPreferenceStore().getBoolean(JUnitSeleniumPage.class.getSimpleName()+".enabledtrustSSL");
	}
	
	public String getPathToProfile(){
		return _pathToProfile;
	}
	
	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 3;
		composite.setLayout(gridLayout);
		
		GridData layoutData = new GridData();

		Label label = new Label(composite, SWT.NONE);
		label.setText("Firefox Profile Template");
		layoutData.horizontalAlignment = SWT.LEFT;
		label.setLayoutData(layoutData);
		
		_pathToProfileField = new Text(composite, SWT.SINGLE | SWT.BORDER);
		_pathToProfileField.setText(_pathToProfile);
		_pathToProfileField.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				_pathToProfile = _pathToProfileField.getText();
				Activator.getDefault().getPreferenceStore().setValue(JUnitSeleniumPage.class.getSimpleName()+".pathToProfile", _pathToProfile);
				dialogChanged();
			}
		});
		
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		_pathToProfileField.setLayoutData(layoutData);
		
		Button browse = new Button(composite, SWT.NONE);
		browse.setText("Browse...");
		
		final DirectoryDialog fileDialog = new DirectoryDialog(getShell());
		
		browse.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {				
				String tmp = fileDialog.open();
				if (tmp != null){
					_pathToProfileField.setText(tmp);
					dialogChanged();
				}
			}
		});
		
		browse.setLayoutData(layoutData);
		
		/******************************************************************/

		/******************************************************************/
		
		layoutData = new GridData();
		_trustSSL = new Button(composite, SWT.CHECK);
		_trustSSL.setSelection(_enabledtrustSSL);
		_trustSSL.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				_enabledtrustSSL = _trustSSL.getSelection();
				Activator.getDefault().getPreferenceStore().setValue(JUnitSeleniumPage.class.getSimpleName()+".enabledtrustSSL",_enabledtrustSSL);
			}
		});
		
		layoutData.horizontalAlignment = SWT.RIGHT;
		_trustSSL.setLayoutData(layoutData);
		
		layoutData = new GridData();
		label = new Label(composite, SWT.NONE);
		label.setText("Trust All SSL Certificates");
		layoutData.horizontalAlignment = SWT.LEFT;
		label.setLayoutData(layoutData);
		/******************************************************************/

		setControl(composite);
		dialogChanged();
	}

	
	private void dialogChanged() {
		String message = null;
		if(_pathToProfile.length()>0 && !new File(_pathToProfile).exists()){
			message = _pathToProfile +" does not exist";
		}
		updateStatus(message);	
	}
	
	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}
	

	public Collection<Parameter> getInitializationParameters() {
		ArrayList<Parameter> parameters = new ArrayList<Parameter>();
		Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
		p.setName("selenium.trustAllSSLCertificates");
		p.setValue(Boolean.toString(_enabledtrustSSL));
		parameters.add(p);
		
		p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
		p.setName("selenium.firefoxProfileTemplate");
		p.setValue(new File(_pathToProfile).getName());
		parameters.add(p);
		
		return parameters;
	}

	public boolean isEnabled() {
		return true;
	}
	

}
