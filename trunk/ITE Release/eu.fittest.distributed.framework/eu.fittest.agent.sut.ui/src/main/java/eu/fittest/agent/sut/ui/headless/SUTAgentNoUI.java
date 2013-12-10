package eu.fittest.agent.sut.ui.headless;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Properties;
import java.util.Scanner;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import eu.fittest.agent.sut.ui.AbstractAgentUI;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;

public class SUTAgentNoUI extends AbstractAgentUI{
	static public final String CONF_FILE=FITTESTConstants.FITTEST_SETTINGS_DIR+"agent.conf";
	static private final String AGENT_TYPE="agent.type";
	static private final String AGENT_ENVIRONMENT="agent.environment";
	static private final String AGENT_DESCRIPTION="agent.description";
	
	private ActionListener _registerAction = null;
	private ActionListener _exitAction = null;
	
	public SUTAgentNoUI(){
		super();
	}
	
	public void open(){
		Properties properties = new Properties();
		try {
			properties.load(new FileInputStream(new File(new URI(CONF_FILE))));
			_description = properties.getProperty(AGENT_DESCRIPTION);
			_type = HUTType.fromValue(properties.getProperty(AGENT_TYPE));
			_env = HUTEnvironment.fromValue(properties.getProperty(AGENT_ENVIRONMENT));
			ITELogger.log(Level.INFO,"agent is :"+_description+", "+_type+", "+_env);
		} catch (FileNotFoundException e) {
			ITELogger.log(Level.SEVERE,e.getMessage());
		} catch (IOException e) {
			ITELogger.log(Level.SEVERE,e.getMessage());
		}  catch (URISyntaxException e) {
			ITELogger.log(Level.SEVERE,e.getMessage());
		}
		
		Scanner in = new Scanner(System.in);	     
	     if(_registerAction!=null) _registerAction.actionPerformed(new ActionEvent(this, 0, "register"));
	     
	     boolean exit = false;
	     while(!exit){
	    	 System.out.println("Press E then Hit Enter to exit:");	   
		     String value = in.next();
		     exit = "E".equalsIgnoreCase(value);
	     }
	     if(_exitAction!=null) _exitAction.actionPerformed(new ActionEvent(this, 0, "exit"));
	}

	@Override
	public void close() {
		
	}

	@Override
	public void addExitActionListener(ActionListener l) {
		_exitAction = l;
	}
	
	@Override
	public void addRegisterActionListener(ActionListener l) {
		_registerAction = l;
	}

	@Override
	public synchronized void incomingEvent(ServiceEvent event) {
		System.out.println(event.getSource());		
	}

	@Override
	public void displayMessage(Level level, String message) {
		ITELogger.log(level, message);
	}

	@Override
	public void exit() {
		// TODO Auto-generated method stub
		
	}
}
