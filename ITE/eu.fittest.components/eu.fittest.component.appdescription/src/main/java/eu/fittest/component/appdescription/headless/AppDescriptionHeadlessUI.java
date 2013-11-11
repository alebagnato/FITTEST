package eu.fittest.component.appdescription.headless;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.util.NetworkUtils;
import eu.fittest.component.appdescription.IAppDescriptionView;
import eu.fittest.component.appdescription.ite.services.properties.spec.AvailableProperties;
import eu.fittest.component.appdescription.utils.InputValidator;

public class AppDescriptionHeadlessUI implements IAppDescriptionView{
	private PropertyChangeSupport _support;
	
	private String _protocol;
	private String _hostIp;
	private Integer _port;
	private String _url;
	
	public String getURL(){
		return _protocol+_hostIp+":"+_port.toString()+"/"+_url;
	}
	
	public AppDescriptionHeadlessUI() {
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
		boolean inputIsCorrect = false;
		String URL = null;
		try{
			Scanner in = new Scanner(System.in);
			
			while(!inputIsCorrect){
				System.out.println("Please enter URL of System Under Test:");
				String prefix = "http://"+NetworkUtils.getLocalIP()+":";
				System.out.print(prefix);
				
				URL = prefix+in.next();
				inputIsCorrect = InputValidator.checkURL(URL);
				if(!inputIsCorrect){
					System.out.println(URL +" is not a valid URL");
				}
				else{
					System.out.println("OK");
				}
			}
			_support.firePropertyChange(AvailableProperties.SELENIUM, null, URL);
			
			String executableJar = null;
			inputIsCorrect = false;
			while(!inputIsCorrect){
				System.out.println("Do you want to specify the path to the server executable jar? (Y/N)");
				String answer = in.next();
				if(answer.equalsIgnoreCase("Y")){
					System.out.println("Path to executable jar:");
					executableJar = in.next();
					if(InputValidator.checkExecutableJar(executableJar)){
						System.out.println("OK");
						inputIsCorrect = true;
						_support.firePropertyChange(AvailableProperties.EXECUTABLE_JAR, null, executableJar);
					}
					else{
						System.out.println("Path is not valid");
					}
				}
				else if(answer.equalsIgnoreCase("N")){
					inputIsCorrect = true;
				}
			}
			
			System.out.println("Done.");
			
		}catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE,e.getMessage());
		}		
	}

}
