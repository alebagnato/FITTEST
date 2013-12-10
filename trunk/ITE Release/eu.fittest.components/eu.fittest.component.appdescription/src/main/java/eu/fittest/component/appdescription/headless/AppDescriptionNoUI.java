package eu.fittest.component.appdescription.headless;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.util.NetworkUtils;
import eu.fittest.component.appdescription.IAppDescriptionView;
import eu.fittest.component.appdescription.constants.IAppDescriptionConstants;
import eu.fittest.component.appdescription.ite.services.properties.spec.AvailableProperties;
import eu.fittest.component.appdescription.utils.InputValidator;

public class AppDescriptionNoUI implements IAppDescriptionView{
	private Properties _properties;
	private PropertyChangeSupport _support;
	
	public AppDescriptionNoUI() {
		_support = new PropertyChangeSupport(this);
	}
	
	@Override
	public void addPropertyChangeListener(PropertyChangeListener l) {
		_support.addPropertyChangeListener(l);
	}

	@Override
	public void removePropertyChangeListener(PropertyChangeListener l) {
		_support.removePropertyChangeListener(l);
	}

	@Override
	public void close() {
		
	}
	
	@Override
	public void open() {
		_properties = new Properties();
		try {
			_properties.load(new FileInputStream(new File(new URI(IAppDescriptionConstants.CONF_FILE))));
			String urlValue = _properties.getProperty(IAppDescriptionConstants.URL);
			if(urlValue == null){
				String protocol = _properties.getProperty(IAppDescriptionConstants.PROTOCOL);
				String port = _properties.getProperty(IAppDescriptionConstants.PORT);
				String path = _properties.getProperty(IAppDescriptionConstants.PATH);
				String ip = NetworkUtils.getLocalIP();
				urlValue = protocol +"://"+ip+":"+port+path;
			}
			Logger.getAnonymousLogger().log(Level.INFO,"value of URL is: "+urlValue);
			if(!InputValidator.checkURL(urlValue)){
				Logger.getAnonymousLogger().log(Level.WARNING,"URL property in "+IAppDescriptionConstants.CONF_FILE+" is not reachable");
			}
			_support.firePropertyChange(AvailableProperties.SELENIUM, null,urlValue);
			
			String executableJar = _properties.getProperty(IAppDescriptionConstants.EXECUTABLE_JAR);
			if(executableJar!=null){
				String[] fileName = executableJar.split(" ");
				if(InputValidator.checkExecutableJar(fileName[0])){
					_support.firePropertyChange(AvailableProperties.EXECUTABLE_JAR, null, executableJar);
				}
				else throw new FITTESTException("Executable jar in "+IAppDescriptionConstants.CONF_FILE+" is not valid: "+ executableJar+"is a wrong path or is not executable");
			}
		} catch (FileNotFoundException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE,e.getMessage());
		} catch (IOException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE,e.getMessage());
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE,e.getMessage());
		} catch (URISyntaxException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE,e.getMessage());
		}
	}

}
