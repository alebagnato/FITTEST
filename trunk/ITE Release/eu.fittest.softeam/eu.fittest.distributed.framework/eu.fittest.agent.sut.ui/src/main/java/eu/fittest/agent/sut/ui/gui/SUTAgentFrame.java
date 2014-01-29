package eu.fittest.agent.sut.ui.gui;

import java.awt.AWTException;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.SystemTray;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextArea;

import eu.fittest.agent.sut.AppLock;
import eu.fittest.agent.sut.ui.IAgentView;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;

public class SUTAgentFrame extends JFrame implements WindowListener, IAgentView{
	private JComboBox _environment;
	private JComboBox _type;
	private JButton _ok;
	private JButton _exit;
	private JTextArea _description;
	private JCheckBox _systrayMessage;
	private AgentTrayIcon _icon = null;
	private ActionListener _exitListener;
	private Preferences _prefs = Preferences.userNodeForPackage(SUTAgentFrame.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	public void addExitActionListener(ActionListener l){
		_exit.addActionListener(l);
		_exitListener =l;
		if(_icon!=null) _icon.addExitListener(l);
	}
	
	public void addRegisterActionListener(ActionListener l){
		_ok.addActionListener(l);
		if(_icon!=null) _icon.addRegisterListener(l);
	}
	
	public SUTAgentFrame() throws FITTESTException {
		super("FITTEST SUT Agent");
		
		
		setSize(300,250);
		getContentPane().setLayout(new GridBagLayout());
		
		GridBagConstraints constraints = new GridBagConstraints(0,0,2,1,1D,1D, GridBagConstraints.CENTER,
				GridBagConstraints.NONE,new Insets(0, 0, 0, 0),0,0);
		JLabel lbl = new JLabel("Select the kind of host you are running:");
		getContentPane().add(lbl, constraints);
		
		lbl = new JLabel("Environment:");
		constraints = new GridBagConstraints(0,1,1,1,1D,1D, GridBagConstraints.WEST,
				GridBagConstraints.NONE,new Insets(0, 0, 0, 0),0,0);
		getContentPane().add(lbl, constraints);
		
		_environment = new JComboBox(HUTEnvironment.values());
		constraints = new GridBagConstraints(1,1,1,1,1D,1D, GridBagConstraints.CENTER,
				GridBagConstraints.HORIZONTAL,new Insets(0, 0, 0, 0),0,0);
		getContentPane().add(_environment, constraints);
		
		_environment.setSelectedIndex(1); // default = Production, by CuND
		
		lbl = new JLabel("HUT Type:");
		constraints = new GridBagConstraints(0,2,1,1,1D,1D, GridBagConstraints.WEST,
				GridBagConstraints.NONE,new Insets(0, 0, 0, 0),0,0);
		getContentPane().add(lbl, constraints);
		
		_type = new JComboBox(HUTType.values());
		constraints = new GridBagConstraints(1,2,1,1,1D,1D, GridBagConstraints.CENTER,
				GridBagConstraints.HORIZONTAL,new Insets(0, 0, 0, 0),0,0);
		getContentPane().add(_type, constraints);
		
		_type.setSelectedIndex(0); // default = Client, by CuND
		
		lbl = new JLabel("Description:");
		constraints = new GridBagConstraints(0,3,1,1,1D,1D, GridBagConstraints.WEST,
				GridBagConstraints.NONE,new Insets(0, 0, 0, 0),0,0);
		getContentPane().add(lbl, constraints);
		
		_description = new JTextArea(System.getProperty("os.name")+", "+System.getProperty("os.version")+", "+System.getProperty("os.arch"));
		constraints = new GridBagConstraints(0,4,2,1,1D,1D, GridBagConstraints.CENTER,
				GridBagConstraints.BOTH,new Insets(0, 0, 0, 0),0,0);
		getContentPane().add(_description, constraints);
		
		// Modified by Cu, set default = false
		_systrayMessage = new JCheckBox("Enable systray messages", _prefs.getBoolean("systraymessages.enabled", false));
		_systrayMessage.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent arg0) {
				_prefs.putBoolean("systraymessages.enabled", _systrayMessage.isSelected());
				try {
					_prefs.flush();
				} catch (BackingStoreException e) {
					ITELogger.log(Level.WARNING, e.getMessage());
				}			
			}
		});
		constraints = new GridBagConstraints(0,5,2,1,1D,1D, GridBagConstraints.CENTER,
				GridBagConstraints.BOTH,new Insets(0, 0, 0, 0),0,0);
		getContentPane().add(_systrayMessage, constraints);
		
		// Disable the element if the user is client
		String uiEnableValue = System.getProperty(
				FITTESTConstants.FITTEST_SUT_AGENT_UI_ENABLED, "false");

		if ("false".equals(uiEnableValue)) {
			_environment.setEnabled(false);
			_type.setEnabled(false);
			_systrayMessage.setEnabled(false);
		}
		
		_ok = new JButton("Register");
		_ok.setActionCommand("register");
		constraints = new GridBagConstraints(0,6,1,1,1D,1D, GridBagConstraints.EAST,
				GridBagConstraints.NONE,new Insets(0, 0, 0, 0),0,0);
		getContentPane().add(_ok, constraints);	
		
		_exit = new JButton("Exit");
		_exit.setActionCommand("exit");
		constraints = new GridBagConstraints(1,6,1,1,1D,1D, GridBagConstraints.WEST,
				GridBagConstraints.NONE,new Insets(0, 0, 0, 0),0,0);
		getContentPane().add(_exit, constraints);
		
//		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		addWindowListener(this);
		addTrayIcon();
		registerListeners();
	}
	
	private void registerListeners(){
		_systrayMessage.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				_icon.setMessageEnabled(_systrayMessage.isSelected());
				
			}
		});
	}
	
	private void addTrayIcon(){
		if(SystemTray.isSupported()){
			SystemTray tray = SystemTray.getSystemTray();			
			_icon = new AgentTrayIcon();
			_icon.setMessageEnabled(_systrayMessage.isSelected());
			try {
				tray.add(_icon);
			} catch (AWTException e) {
				ITELogger.log(Level.WARNING, e.getMessage());
			}
		}
	}
	
	public void exit(){
		SystemTray.getSystemTray().remove(_icon);
		dispose();
	}

	
	public void windowActivated(WindowEvent arg0) {
	}

	
	public void windowClosed(WindowEvent arg0) {
	}

	
	public void windowClosing(WindowEvent arg0) {
//		if(_exitListener!=null){
//			_exitListener.actionPerformed(new ActionEvent(this, 0, "exit"));
//		}
	}
	
	
	
	public void windowDeactivated(WindowEvent arg0) {
		
	}

	
	public void windowDeiconified(WindowEvent arg0) {
		
	}

	
	public void windowIconified(WindowEvent arg0) {
		
	}

	
	public void windowOpened(WindowEvent arg0) {
		
	}

	@Override
	public HUTEnvironment getHUTEnvironment() {
		return (HUTEnvironment)_environment.getSelectedItem();
	}
	
	public void setHUTEnvironment(HUTEnvironment environment){
		_environment.setSelectedItem(environment);
	}

	@Override
	public HUTType getHUTType() {
		return (HUTType)_type.getSelectedItem();
	}
	
	public void setHUTType(HUTType type){
		_type.setSelectedItem(type);
	}

	@Override
	public String getDescription() {
		return  _description.getText();
	}
	
	public void setDescription(String text){
		_description.setText(text);
	}

	@Override
	public void open() {
		setVisible(true);		
	}

	@Override
	public void close() {
		setVisible(false);	
		if(_icon!=null){
			_icon.setToolTip(getHUTEnvironment()+" "+getHUTType()+"\n"+getDescription());
		}
	}

	@Override
	public synchronized void incomingEvent(ServiceEvent event) {
		if (_icon != null)
			_icon.incomingEvent(event);
	}

	@Override
	public void displayMessage(Level level, String message) {
		if(level.intValue()==Level.SEVERE.intValue()){
			JOptionPane.showMessageDialog(this, message, Level.SEVERE.getName(), JOptionPane.ERROR_MESSAGE);
		}
		else if(level.intValue()==Level.INFO.intValue()){
			JOptionPane.showMessageDialog(this, message, Level.INFO.getName(), JOptionPane.INFORMATION_MESSAGE);
		}
	}
}
