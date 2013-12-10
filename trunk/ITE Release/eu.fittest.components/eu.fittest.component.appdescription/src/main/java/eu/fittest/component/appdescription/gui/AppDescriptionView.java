package eu.fittest.component.appdescription.gui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.text.NumberFormat;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.util.NetworkUtils;
import eu.fittest.component.appdescription.AppDescriptionComponent;
import eu.fittest.component.appdescription.IAppDescriptionView;
import eu.fittest.component.appdescription.constants.IAppDescriptionConstants;
import eu.fittest.component.appdescription.ite.services.properties.spec.AvailableProperties;
import eu.fittest.component.appdescription.utils.ExecutableJarFilter;
import eu.fittest.component.appdescription.utils.InputValidator;

public class AppDescriptionView extends JFrame implements PropertyChangeListener, IAppDescriptionView{
	private static final long serialVersionUID = 1L;
	private JFormattedTextField _port;
	private JTextField _protocol;
	private JTextField _url;
	private JTextField _host;
	private JLabel _checkedProtocol;
	private JLabel _checkedPath;
	private Preferences _prefs = Preferences.userNodeForPackage(AppDescriptionView.class);
	
	private JTextField _pathToJar;
	
	private JFileChooser _filechooser;
	
	private PropertyChangeSupport _support;
	
	private String _strUrl;
	private String _strPathToJar;

	private String computeURL(){
		return _protocol.getText()+_host.getText()+":"+_port.getValue().toString()+"/"+_url.getText();
	}
	
	public void addPropertyChangeListener(PropertyChangeListener l){
		_support.addPropertyChangeListener(l);
		_support.firePropertyChange(AvailableProperties.SELENIUM,null, _strUrl);
		_support.firePropertyChange(AvailableProperties.EXECUTABLE_JAR,null, _strPathToJar);
	}
	
	public void removePropertyChangeListener(PropertyChangeListener l){
		_support.removePropertyChangeListener(l);
	}
	
	public AppDescriptionView() throws FITTESTException {
		super(AppDescriptionComponent.class.getName());	
		_strPathToJar = _prefs.get(AvailableProperties.EXECUTABLE_JAR, null);
		_strUrl = _prefs.get(AvailableProperties.SELENIUM, null);
		_support = new PropertyChangeSupport(this);
		
		getContentPane().setLayout(new GridBagLayout());
		
		///////////////////////////////1st LINE/////////////////////////////////////////////////////////////////////
		GridBagConstraints c = new GridBagConstraints(0,0,1,1,1.0,1.0,GridBagConstraints.LINE_START,
				GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		
		_protocol = new JTextField("http://");
		_protocol.setEditable(false);
		getContentPane().add(_protocol,c);
		
		 c = new GridBagConstraints(1,0,1,1,1.0,1.0,GridBagConstraints.LINE_START,
					GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		_host = new JTextField(); 
		_host.setText(_prefs.get(IAppDescriptionConstants.HOST, NetworkUtils.getLocalIP()));
		_host.setEditable(true);
		getContentPane().add(_host,c);

		 c = new GridBagConstraints(2,0,1,1,1.0,1.0,GridBagConstraints.LINE_START,
					GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		JTextField sep = new JTextField(":");
		sep.setEditable(false);
		getContentPane().add(sep,c);
		
		 c = new GridBagConstraints(3,0,1,1,1.0,1.0,GridBagConstraints.LINE_START,
					GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		NumberFormat format = NumberFormat.getIntegerInstance();
		format.setGroupingUsed(false);
		_port = new JFormattedTextField(format);
		_port.setColumns(4);
		_port.setValue(_prefs.getInt(IAppDescriptionConstants.PORT, 80));
		getContentPane().add(_port,c);
		
		 c = new GridBagConstraints(4,0,1,1,1.0,1.0,GridBagConstraints.LINE_START,
					GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		JTextField separator = new JTextField("/");
		separator.setEditable(false);
		getContentPane().add(separator,c);
		
		 c = new GridBagConstraints(5,0,1,1,1.0,1.0,GridBagConstraints.LINE_START,
					GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		_url = new JTextField();
		_url.setToolTipText("<path to application>");
		_url.setPreferredSize(new Dimension(300, 20));
		_url.setText(_prefs.get(IAppDescriptionConstants.PATH, null));
		getContentPane().add(_url,c);
		
		 c = new GridBagConstraints(6,0,1,1,1.0,1.0,GridBagConstraints.LINE_START,
					GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		 _checkedProtocol = new JLabel();
		getContentPane().add(_checkedProtocol,c);
		
		
		///////////////////////////////2nd LINE/////////////////////////////////////////////////////////////////////
		c = new GridBagConstraints(0,1,3,1,1.0,1.0,GridBagConstraints.LINE_START,
				GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		
		JLabel jarLbl = new JLabel("Path to Application Jar (IBM-Contest, optional):");
		getContentPane().add(jarLbl,c);
		
		c = new GridBagConstraints(3,1,3,1,1.0,1.0,GridBagConstraints.LINE_START,
				GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		_pathToJar = new JTextField();
		_pathToJar.setText(_strPathToJar);
		_pathToJar.setToolTipText("Use the Browse button to select an executable jar, then add the jar parameter values at the end of this field");
		_pathToJar.addKeyListener(new ExecutableJarKeyListener());
		_pathToJar.setPreferredSize(new Dimension(300,20));
		getContentPane().add(_pathToJar,c);
		
		
		
		 c = new GridBagConstraints(6,1,1,1,1.0,1.0,GridBagConstraints.LINE_START,
					GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		 _checkedPath = new JLabel();
		getContentPane().add(_checkedPath,c);
		
		c = new GridBagConstraints(7,1,1,1,1.0,1.0,GridBagConstraints.LINE_START,
				GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		JButton browse = new JButton("Browse...");
		
		_filechooser = new JFileChooser(_prefs.get(IAppDescriptionConstants.FOLDER, null));	
		_filechooser.setFileFilter(new ExecutableJarFilter());
		
		browse.addActionListener(new FileBrowserAction(this));
		getContentPane().add(browse,c);
		
		setCheckStatus(_checkedPath, false);
		pack();
		setDefaultCloseOperation(HIDE_ON_CLOSE);
		
		registerListeners();
		
		propertyChange(null);
	}
	
	private void registerListeners(){
		_url.addKeyListener(new URLKeyListener());
		_port.addPropertyChangeListener("value", this);
	}

	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		if(InputValidator.checkURL(computeURL())){
			String old = _strUrl;
			_strUrl = computeURL();
			setCheckStatus(_checkedProtocol, true);
			_prefs.put(IAppDescriptionConstants.PORT, _port.getValue().toString());
			_prefs.put(IAppDescriptionConstants.HOST, _host.getText());
			_prefs.put(IAppDescriptionConstants.URL, _url.getText());
			_support.firePropertyChange(AvailableProperties.SELENIUM, old, _strUrl);
		}
		else{
			setCheckStatus(_checkedProtocol, false);
		}
		String[] pathToJar = _pathToJar.getText().split(" ");
		if(InputValidator.checkExecutableJar(pathToJar[0])){
			setCheckStatus(_checkedPath, true);
		}
		else{
			setCheckStatus(_checkedPath, false);
		}
	}
	
	private void setCheckStatus(JLabel source, boolean ok){
		if(ok){
			source.setIcon(IAppDescriptionConstants.OK);
		}
		else {
			source.setIcon(IAppDescriptionConstants.NOT_OK);
		}
	}

	@Override
	public void close() {
		dispose();		
	}

	@Override
	public void open() {
		setVisible(true);		
	}
	
	private class FileBrowserAction implements ActionListener{
		private Component _c;
		public FileBrowserAction(Component c) {
			_c = c;
		}
		@Override
		public void actionPerformed(ActionEvent e) {
			if(_filechooser.showOpenDialog(_c)== JFileChooser.APPROVE_OPTION){
				_prefs.put(IAppDescriptionConstants.FOLDER,_filechooser.getCurrentDirectory().getAbsolutePath());
				try {
					_prefs.flush();
				} catch (BackingStoreException e1) {
					Logger.getAnonymousLogger().log(Level.WARNING,e1.getMessage());
				}
				String old  = _strPathToJar;
				_pathToJar.setText(_filechooser.getSelectedFile().getAbsolutePath());
				_strPathToJar = _pathToJar.getText();
				setCheckStatus(_checkedPath, true);
				_prefs.put(AvailableProperties.EXECUTABLE_JAR, _strPathToJar);
				_support.firePropertyChange(AvailableProperties.EXECUTABLE_JAR, old, _strPathToJar);
			}
		}		
	}
	
	private class URLKeyListener implements KeyListener{
		@Override
		public void keyPressed(KeyEvent e) {
		}

		@Override
		public void keyReleased(KeyEvent e) {
			if(InputValidator.checkURL(computeURL())){
				String old = _strUrl;
				_strUrl = computeURL();
				setCheckStatus(_checkedProtocol,true);
				_prefs.put(IAppDescriptionConstants.PORT, _port.getValue().toString());
				_prefs.put(IAppDescriptionConstants.PATH, _url.getText());
				_prefs.put(IAppDescriptionConstants.HOST, _host.getText());
				_support.firePropertyChange(AvailableProperties.SELENIUM, old, _strUrl);
			}
			else{
				setCheckStatus(_checkedProtocol, false);
			}
		}

		@Override
		public void keyTyped(KeyEvent e) {

		}
	}
	
	private class ExecutableJarKeyListener implements KeyListener{

		@Override
		public void keyPressed(KeyEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void keyReleased(KeyEvent event) {
			String[] pathToJar = _pathToJar.getText().split(" ");
			if(InputValidator.checkExecutableJar(pathToJar[0])){
				setCheckStatus(_checkedPath,true);
				String old = _strPathToJar;
				_strPathToJar = _pathToJar.getText();
				_prefs.put(AvailableProperties.EXECUTABLE_JAR, _strPathToJar);
				_support.firePropertyChange(AvailableProperties.EXECUTABLE_JAR, old, _strPathToJar);
			}
			else{
				setCheckStatus(_checkedPath,false);
			}		
		}

		@Override
		public void keyTyped(KeyEvent arg0) {
			// TODO Auto-generated method stub
			
		}
		
	}

}
