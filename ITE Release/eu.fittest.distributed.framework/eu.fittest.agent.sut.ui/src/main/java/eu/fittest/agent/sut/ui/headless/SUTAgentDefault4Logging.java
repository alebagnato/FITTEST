package eu.fittest.agent.sut.ui.headless;

import java.awt.AWTException;
import java.awt.SystemTray;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import eu.fittest.agent.sut.ui.AbstractAgentUI;
import eu.fittest.agent.sut.ui.gui.AgentTrayIcon;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;

public class SUTAgentDefault4Logging extends AbstractAgentUI {

	private ActionListener registerAction = null;
	private ActionListener exitAction = null;
	private AgentTrayIcon _icon = null;

	public SUTAgentDefault4Logging() {
		super();
	}

	@Override
	public void open() {
		_description = "Default logging agent: " + System.getProperty("os.name") +
				", " + System.getProperty("os.version") +
				", " + System.getProperty("os.arch");
		_type = HUTType.CLIENT;
		_env = HUTEnvironment.PRODUCTION;
		ITELogger.log(Level.INFO,
				"agent is :" + _description + ", " + _type + ", " + _env);

//		addTrayIcon();
		
		// Auto register 
		if (registerAction != null)
			registerAction.actionPerformed(new ActionEvent(this, 0, "register"));
	
	}
	
	private void addTrayIcon(){
		if(SystemTray.isSupported()){
			SystemTray tray = SystemTray.getSystemTray();			
			_icon  = new AgentTrayIcon();
			_icon.setMessageEnabled(false);
			try {
				tray.add(_icon);
			} catch (AWTException e) {
				ITELogger.log(Level.WARNING, e.getMessage());
			}
		}
	}

	@Override
	public void close() {

	}

	@Override
	public void addExitActionListener(ActionListener l) {
		exitAction = l;
		if(_icon!=null) _icon.addExitListener(l);
	}

	@Override
	public void addRegisterActionListener(ActionListener l) {
		registerAction = l;
		if(_icon!=null) _icon.addRegisterListener(l);
	}

	@Override
	public synchronized void incomingEvent(ServiceEvent event) {
//		System.out.println(event.getSource());
		if (_icon != null)
			_icon.incomingEvent(event);
	}

	@Override
	public void displayMessage(Level level, String message) {
		ITELogger.log(level, message);
	}

	@Override
	public void exit() {
		if (_icon != null)
			SystemTray.getSystemTray().remove(_icon);
	}
}
