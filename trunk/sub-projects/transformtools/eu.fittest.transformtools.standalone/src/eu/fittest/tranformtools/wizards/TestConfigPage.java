package eu.fittest.tranformtools.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class TestConfigPage extends WizardPage {

	public String seleniumBrowserStartCommand = "*chome";
	public String seleniumServerHost= "localhost";
	public String seleniumServerPort= "4444";
	public String seleniumSpeed="100";

	public String applicationName="";
	
	protected TestConfigPage(String pageName) {
		super(pageName);
		
		applicationName = StorePreferences.getSavedValue("TestConfigPage_applicationName");
	}

	
	public void createControl(Composite parent) {
		
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(2, false);
		composite.setLayout(layout);
		
		// Browser start command
		Label label = new Label(composite, SWT.NONE);
		label.setText("Selenium Browser Start Command: ");
  		final Text textBrowserStartCommand = new Text(composite, SWT.BORDER | SWT.SINGLE);
  		textBrowserStartCommand.setText(seleniumBrowserStartCommand);
  		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
  		textBrowserStartCommand.setLayoutData(layoutData);
  		
  		textBrowserStartCommand.addModifyListener(new ModifyListener() {
			
			public void modifyText(ModifyEvent e) {
				seleniumBrowserStartCommand = textBrowserStartCommand.getText();
			}
		});
		
		// host
  		label = new Label(composite, SWT.NONE);
		label.setText("Selenium Host: ");
  		final Text textHost = new Text(composite, SWT.BORDER | SWT.SINGLE);
  		textHost.setText(seleniumServerHost);
  		layoutData = new GridData(GridData.FILL_HORIZONTAL);
  		textHost.setLayoutData(layoutData);
  		
  		textHost.addModifyListener(new ModifyListener() {
			
			
			public void modifyText(ModifyEvent e) {
				seleniumServerHost = textHost.getText();
				
			}
		});
		
  		// port
  		label = new Label(composite, SWT.NONE);
  		label.setText("Selenium Port: ");
  		final Text textPort = new Text(composite, SWT.BORDER | SWT.SINGLE);
  		textPort.setText(seleniumServerPort);
  		layoutData = new GridData(GridData.FILL_HORIZONTAL);
  		textPort.setLayoutData(layoutData);
  		
  		textPort.addModifyListener(new ModifyListener() {
			
			public void modifyText(ModifyEvent e) {
				seleniumServerPort = textPort.getText();
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

  		// application id
  		label = new Label(composite, SWT.NONE);
  		label.setText("Flex/Flash Object ID: ");
  		final Text textAppID = new Text(composite, SWT.BORDER | SWT.SINGLE);
  		textAppID.setText(applicationName);
  		layoutData = new GridData(GridData.FILL_HORIZONTAL);
  		textAppID.setLayoutData(layoutData);
  		
  		textAppID.addModifyListener(new ModifyListener() {
  			
  			public void modifyText(ModifyEvent e) {
  				applicationName = textAppID.getText();
  				StorePreferences.saveValue("TestConfigPage_applicationName",applicationName);
  			}
  		});
  		
  		setControl(composite);
	}

}
