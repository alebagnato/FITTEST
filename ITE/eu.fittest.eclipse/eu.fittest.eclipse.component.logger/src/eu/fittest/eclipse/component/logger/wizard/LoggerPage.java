package eu.fittest.eclipse.component.logger.wizard;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Text;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.component.logger.Activator;


public class LoggerPage extends WizardPage implements IFITTESTComponentWizardPage{
	private Scale _loggingLevel;
	private Text _description;
	private int _loggingLevelValue;
	
	private Button _enabled;
	private boolean _enabledValue = true;

	private static final Properties LEVEL_DESCRIPTIONS = new Properties();
	
	static{
		try {
			LEVEL_DESCRIPTIONS.load(new FileInputStream(Activator.getResource("resources/level.properties")));
		} catch (FileNotFoundException e) {
			Logger.getAnonymousLogger().log(Level.WARNING, e.getMessage());
		} catch (IOException e) {
			Logger.getAnonymousLogger().log(Level.WARNING, e.getMessage());
		} catch (URISyntaxException e) {
			Logger.getAnonymousLogger().log(Level.WARNING, e.getMessage());
		}
	}
	
	public LoggerPage(boolean enabled, int level) {
		super("Logger initialization parameters");
		_enabledValue = enabled;
		_loggingLevelValue = level;
		setTitle("Logger initialization parameters");
	}
	
	private void enableContent() {
		_loggingLevel.setEnabled(_enabledValue);
		_description.setEnabled(_enabledValue);		
	}

	private void dialogChanged(){
		if(!_enabled.getSelection()){
			updateStatus(null);	
		}
	}
	
	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}
	
	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NULL); // Fixed by Cu to work on Linux
//		Composite composite = new Composite(parent, SWT.EMBEDDED);
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
		_enabled.setSelection(_enabledValue);
		
		layoutData = new GridData();
		Label label = new Label(composite, SWT.NONE);
		label.setText("Enable Flash logger component");
		layoutData.horizontalAlignment = SWT.LEFT;
		label.setLayoutData(layoutData);
		
		label = new Label(composite, SWT.NONE);
		label.setText("Level of details:");
		
		layoutData = new GridData();
		layoutData.horizontalAlignment = SWT.CENTER;
		layoutData.horizontalSpan = 3;
		layoutData.grabExcessHorizontalSpace = true;
		label.setLayoutData(layoutData);
		
		label = new Label(composite, SWT.NONE);
		label.setText("Min");
		layoutData = new GridData();
		layoutData.horizontalAlignment = SWT.BEGINNING;
		layoutData.horizontalSpan = 1;
		layoutData.grabExcessHorizontalSpace = false;
		label.setLayoutData(layoutData);
		
		_loggingLevel = new Scale (composite, SWT.HORIZONTAL);
		_loggingLevel.setMaximum (LEVEL_DESCRIPTIONS.size());
		_loggingLevel.setMinimum(1);
		_loggingLevel.setPageIncrement(1);
		_loggingLevel.setIncrement(1);
		
		layoutData = new GridData();
		layoutData.horizontalAlignment = SWT.FILL;
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalSpan = 1;
		_loggingLevel.setLayoutData(layoutData);
		
		_loggingLevel.addSelectionListener(new SelectionListener() {		
			
			@Override
			public void widgetSelected(SelectionEvent e) {	
				_loggingLevelValue = _loggingLevel.getSelection();
				_description.setText(getDescription(_loggingLevel.getSelection()));
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				
			}
		});
	
		
		label = new Label(composite, SWT.NONE);
		label.setText("Max");
		layoutData = new GridData();
		layoutData.horizontalAlignment = SWT.END;
		layoutData.grabExcessHorizontalSpace = false;
		layoutData.horizontalSpan = 1;
		label.setLayoutData(layoutData);
		
		
		_description = new Text(composite, SWT.BORDER);		
		_description.setEditable(false);
		layoutData = new GridData();
		layoutData.horizontalAlignment = SWT.FILL;
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalSpan = 3;
		layoutData.grabExcessVerticalSpace=true;
		layoutData.verticalAlignment = SWT.FILL;
		_description.setLayoutData(layoutData);
		
		setControl(composite);
		
		_loggingLevel.setSelection(_loggingLevelValue);
		_description.setText(getDescription(_loggingLevel.getSelection()));
		enableContent();
	}

	public Collection<Parameter> getInitializationParameters() {
		ArrayList<Parameter> parameters = new ArrayList<Parameter>();
		Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
		p.setName("loggingLevel");
		p.setValue(getLevel(_loggingLevelValue));
		parameters.add(p);
		return parameters;
	}
	
	private String getDescription(int selection) {
		String desc = null;
		int reverseSelection = LEVEL_DESCRIPTIONS.size() - selection + 1;
		Iterator<Object> keys = LEVEL_DESCRIPTIONS.keySet().iterator();
		while(desc==null && keys.hasNext()){
			String key = keys.next().toString();
			String[] splittedKey = key.split("\\.");
			if(splittedKey[0].equals(Integer.toString(reverseSelection))){
				desc = LEVEL_DESCRIPTIONS.getProperty(key);
			}
			
		}
		return desc;
	}
	
	private String getLevel(int selection) {
		String level = null;
		int reverseSelection = LEVEL_DESCRIPTIONS.size() - selection + 1;
		Iterator<Object> keys = LEVEL_DESCRIPTIONS.keySet().iterator();
		while(level==null && keys.hasNext()){
			String key = keys.next().toString();
			String[] splittedKey = key.split("\\.");
			if(splittedKey[0].equals(Integer.toString(reverseSelection))){
				level = splittedKey[2];
			}			
		}
		return level;
	}

	public boolean isEnabled() {
		return _enabledValue;
	}

}
