package eu.fittest.eclipse.component.junit.wizard;

import java.util.Collection;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.component.junit.Activator;

public class TestConfigPage extends WizardPage implements IFITTESTComponentWizardPage{

	public String seleniumSpeed="100";

	public String loadingTime="3000";

	public String applicationName="";

	public String libraries="";
	
	public TestConfigPage() {
		super("SUT configuration");
		setTitle("SUT configuration");
		
		applicationName = Activator.getDefault().getPreferenceStore().getString("TestConfigPage_applicationName");
		libraries = Activator.getDefault().getPreferenceStore().getString("TestConfigPage_applicationLibraries");
	}

	private void dialogChanged() {
		if(applicationName.length()<1){
			updateStatus("Please provide the name of the application");
		}
		else {
			updateStatus(null);
		}
	}
	
	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}
	
	public void createControl(Composite parent) {
		
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(2, false);
		composite.setLayout(layout);
		
		// Browser start command
		Label label = new Label(composite, SWT.NONE);
		label.setText("Application name: ");
  		final Text textBrowserStartCommand = new Text(composite, SWT.BORDER | SWT.SINGLE);
  		textBrowserStartCommand.setText(applicationName);
  		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
  		textBrowserStartCommand.setLayoutData(layoutData);
  		
  		textBrowserStartCommand.addModifyListener(new ModifyListener() {
			
			public void modifyText(ModifyEvent e) {
				applicationName = textBrowserStartCommand.getText();
				Activator.getDefault().getPreferenceStore().setValue("TestConfigPage_applicationName",applicationName);
				dialogChanged();
			}
		});
		
   		// libs
  		label = new Label(composite, SWT.NONE);
  		label.setText("SWF libraries: ");
  		final Text textLibs = new Text(composite, SWT.BORDER | SWT.SINGLE);
  		textLibs.setText(libraries);
  		layoutData = new GridData(GridData.FILL_HORIZONTAL);
  		textLibs.setLayoutData(layoutData);
  		textLibs.setToolTipText("The SWF libraries containing the Automation Delegates");
  		textLibs.addModifyListener(new ModifyListener() {
  			
  			public void modifyText(ModifyEvent e) {
  				libraries = textLibs.getText();
  				Activator.getDefault().getPreferenceStore().setValue("TestConfigPage_applicationLibraries",libraries);
  			}
  		});
  		
   		// speed
  		label = new Label(composite, SWT.NONE);
  		label.setText("Selenium Run Speed: ");
  		final Text textSpeed = new Text(composite, SWT.BORDER | SWT.SINGLE);
  		textSpeed.setText(seleniumSpeed);
  		layoutData = new GridData(GridData.FILL_HORIZONTAL);
  		textSpeed.setLayoutData(layoutData);
  		
  		textSpeed.addModifyListener(new ModifyListener() {
  			
  			public void modifyText(ModifyEvent e) {
  				seleniumSpeed = textSpeed.getText();
  			}
  		});

  		// loading time
  		label = new Label(composite, SWT.NONE);
  		label.setText("Time to wait for the SUT to load: ");
  		final Text textLoadingTime = new Text(composite, SWT.BORDER | SWT.SINGLE);
  		textLoadingTime.setText(loadingTime);
  		layoutData = new GridData(GridData.FILL_HORIZONTAL);
  		textLoadingTime.setLayoutData(layoutData);
  		
  		textLoadingTime.addModifyListener(new ModifyListener() {
  			
  			public void modifyText(ModifyEvent e) {
  				loadingTime = textLoadingTime.getText();
  			}
  		});
  		
  		dialogChanged();
  		setControl(composite);
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public Collection<Parameter> getInitializationParameters() {
		return null;
	}

}
