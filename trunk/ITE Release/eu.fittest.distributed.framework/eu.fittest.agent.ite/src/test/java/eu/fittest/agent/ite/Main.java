package eu.fittest.agent.ite;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.Map.Entry;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.DefaultListModel;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;

import eu.fittest.agent.ite.core.ITEAgent;
import eu.fittest.agent.ite.services.agentcommand.spec.IAgentCommandService;
import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.agent.ite.services.registration.spec.RegistrationEvent;
import eu.fittest.agent.ite.view.FileChooserView;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.FITTESTAgent;
import eu.fittest.common.core.identity.spec.FITTESTComponent;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.IServiceListener;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.DeployComponent;
import eu.fittest.common.core.xml.Execute;
import eu.fittest.common.core.xml.Initialize;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.common.core.xml.Start;
import eu.fittest.common.core.xml.Stop;
import eu.fittest.common.core.xml.Terminate;
import eu.fittest.common.core.xml.Upload;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;

public class Main extends JFrame implements IServiceListener<ServiceEvent<?>>{	
	/**
	 * 
	 */
	class PopupListener extends MouseAdapter {
	    public void mousePressed(MouseEvent e) {
	        maybeShowPopup(e);
	    }

	    public void mouseReleased(MouseEvent e) {
	        maybeShowPopup(e);
	    }

	    private void maybeShowPopup(MouseEvent e) {
	        if (e.isPopupTrigger()) {
	        	updateUI();
	            _menu.show(e.getComponent(), e.getX(), e.getY());
	        }
	    }
	}
	
	private enum State{init,start,stop,term};
	
	private static final long serialVersionUID = 1L;
	private ITEAgent _agent;
	private JList _list;
	private DefaultListModel _model;
	private JPopupMenu _menu;
	
	private static JFileChooser _fileChooser = new JFileChooser();
	

	
	private void updateUI(){
		_menu.removeAll();	
		
		if(_list.getSelectedValue() instanceof FITTESTComponent){
			JMenuItem initialize = new JMenuItem("Initialize");
			_menu.add(initialize);
			initialize.addActionListener(new ActionListener() {
				
				public void actionPerformed(ActionEvent e) {
					String parameters = JOptionPane.showInputDialog("Give the list of the parameters (e.g. param1=value1, param2=value2):");
					if(parameters!=null){
						Initialize message = FITTESTSingleton.getObjectFactory().createInitialize();
						message.setTo(_list.getSelectedValue().toString());
						Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
						p.setName("session.name");
						p.setValue("testingsession1");
						message.getParameter().add(p);
						
						for(String s: parameters.split(",")){
							String[] chain = s.trim().split("=");
							if(chain.length==2){
								p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
								p.setName(chain[0]);
								p.setValue(chain[1]);
								message.getParameter().add(p);
							}
						}
						
						try {
							_agent.getServiceRegistry().findService(IConnectionService.class).sendMessage(message);
						} catch (FITTESTException e1) {
							e1.printStackTrace();
						}
					}
				}
			});
			
			JMenuItem start = new JMenuItem("Start");
			_menu.add(start);
			start.addActionListener(new ActionListener() {
				
				public void actionPerformed(ActionEvent e) {
					Start message = FITTESTSingleton.getObjectFactory().createStart();
					message.setTo(_list.getSelectedValue().toString());
					try {
						_agent.getServiceRegistry().findService(IConnectionService.class).sendMessage(message);
					} catch (FITTESTException e1) {
						e1.printStackTrace();
					}
					
				}
			});
			
			JMenuItem stop = new JMenuItem("Stop");
			_menu.add(stop);
			stop.addActionListener(new ActionListener() {
				
				public void actionPerformed(ActionEvent e) {
					Stop message = FITTESTSingleton.getObjectFactory().createStop();
					message.setTo(_list.getSelectedValue().toString());
					try {
						_agent.getServiceRegistry().findService(IConnectionService.class).sendMessage(message);
					} catch (FITTESTException e1) {
						e1.printStackTrace();
					}
					
				}
			});
			
			JMenuItem terminate = new JMenuItem("Terminate");
			_menu.add(terminate);
			terminate.addActionListener(new ActionListener() {
				
				public void actionPerformed(ActionEvent e) {
					Terminate message = FITTESTSingleton.getObjectFactory().createTerminate();
					message.setTo(_list.getSelectedValue().toString());
					try {
						_agent.getServiceRegistry().findService(IConnectionService.class).sendMessage(message);
					} catch (FITTESTException e1) {
						e1.printStackTrace();
					}
					
				}
			});
			
			
			/*switch(state){
			case init:
				_menu.getComponent(0).setEnabled(true);
				_menu.getComponent(1).setEnabled(false);
				_menu.getComponent(2).setEnabled(false);
				_menu.getComponent(3).setEnabled(false);
				break;
			case start:
				_menu.getComponent(0).setEnabled(false);
				_menu.getComponent(1).setEnabled(true);
				_menu.getComponent(2).setEnabled(false);
				_menu.getComponent(3).setEnabled(true);
				break;
			case stop:
				_menu.getComponent(0).setEnabled(false);
				_menu.getComponent(1).setEnabled(false);
				_menu.getComponent(2).setEnabled(true);
				_menu.getComponent(3).setEnabled(true);
				break;
			case term:
				_menu.getComponent(0).setEnabled(false);
				_menu.getComponent(1).setEnabled(true);
				_menu.getComponent(2).setEnabled(false);
				_menu.getComponent(3).setEnabled(true);
				break;
			}*/
		}
		else if(_list.getSelectedValue() instanceof FITTESTAgent){
			JMenuItem seleniumRC = new JMenuItem("Start Selenium RC");
			_menu.add(seleniumRC);
			seleniumRC.addActionListener(new ActionListener() {
				
				public void actionPerformed(ActionEvent e) {
					Execute message = FITTESTSingleton.getObjectFactory().createExecute();
					message.setCommand("java -jar selenium-server.jar");
					message.setTo(_list.getSelectedValue().toString());
					try {
						_agent.getServiceRegistry().findService(IConnectionService.class).sendMessage(message);
					} catch (FITTESTException e1) {
						e1.printStackTrace();
					}
					
				}
			});
			
			JMenuItem stopSeleniumRC = new JMenuItem("Stop Selenium RC");
			_menu.add(stopSeleniumRC);
			stopSeleniumRC.addActionListener(new ActionListener() {
				
				public void actionPerformed(ActionEvent e) {
					IAgentCommandService agentService = _agent.getServiceRegistry().findService(IAgentCommandService.class);
					for(Entry<String, Vector<String>> entry : agentService.getAllRunningCommands().entrySet()){
						for(String p: entry.getValue()){
							try {
								agentService.killCommand(entry.getKey(), p);
							} catch (FITTESTException e1) {
								e1.printStackTrace();
							}
						}
					}
					
					
				}
			});
			
			JMenuItem deploy = new JMenuItem("Deploy FITTEST component");
			_menu.add(deploy);
			deploy.addActionListener(new ActionListener() {
				
				public void actionPerformed(ActionEvent e) {										
					if(_fileChooser.showOpenDialog(null)== JFileChooser.APPROVE_OPTION){
						File file = _fileChooser.getSelectedFile();
						Upload upload = FITTESTSingleton.getObjectFactory().createUpload();
						upload.setTo(_list.getSelectedValue().toString());
						upload.setFrom(_agent.getServiceRegistry().findService(IIdentityService.class).getMyIdentity());
						upload.setResource("component/"+file.getName());
						upload.setResourceSize(file.length());
						try {
							_agent.getServiceRegistry().findService(IConnectionService.class).sendMessage(upload);
							_agent.getServiceRegistry().findService(IFileTransferService.class).
							download(_agent.getServiceRegistry().findService(IConnectionService.class).getConnection(upload.getTo()).getOutputStream(), file.toURI().toString());
						} catch (FITTESTException e1) {
							e1.printStackTrace();
						}
						
						DeployComponent deploy = FITTESTSingleton.getObjectFactory().createDeployComponent();
						deploy.setResource(file.getName());
						deploy.setFrom(upload.getFrom());
						deploy.setTo(upload.getTo());
						try {
							_agent.getServiceRegistry().findService(IConnectionService.class).sendMessage(deploy);
						} catch (FITTESTException e1) {
							e1.printStackTrace();
						}
					}
				}
			});
		}
	}
	
	public Main() throws FITTESTException {
		super("FITTEST ITE Test Frame");
		
		addMenuBar();
		
		_agent = new ITEAgent();
		_agent.getServiceRegistry().findService(IRegistrationService.class).addServiceListener(this);
		_agent.start();
		
		_model = new DefaultListModel();
		_list = new JList(_model);
		getContentPane().add(_list);
		setSize(Toolkit.getDefaultToolkit().getScreenSize());
		setVisible(true);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		
		_menu = new JPopupMenu();	
		_list.addMouseListener(new PopupListener());
	}
	
	private void addMenuBar(){
		JMenuBar bar = new JMenuBar();
		JMenu file = new JMenu("File");
		bar.add(file);
		
		JMenuItem instrument = new JMenuItem("Instrument...");
		instrument.addActionListener(FileChooserView.getInstance());
		file.add(instrument);
		
		setJMenuBar(bar);
	}
	
	public static void main(String[] args) throws FITTESTException {
		if(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_JNLP_FILENAME)==null)
			System.setProperty(FITTESTConstants.FITTEST_SUT_AGENT_JNLP_FILENAME, FITTESTConstants.DEFAULT_FITTEST_SUT_AGENT_JNLP_FILENAME);	
		
		if(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_JAR_FILENAME)==null)
			System.setProperty(FITTESTConstants.FITTEST_SUT_AGENT_JAR_FILENAME, FITTESTConstants.DEFAULT_FITTEST_SUT_AGENT_JAR_FILENAME);	

		if(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_MAINCLASS_NAME)==null)
			System.setProperty(FITTESTConstants.FITTEST_SUT_AGENT_JAR_FILENAME, FITTESTConstants.DEFAULT_FITTEST_SUT_AGENT_MAINCLASS);
		
		if(System.getProperty(FITTESTConstants.FITTEST_ITE_HTTP_DIR)==null)
			System.setProperty(FITTESTConstants.FITTEST_ITE_HTTP_DIR, FITTESTConstants.DEFAULT_FITTEST_ITE_HTTP_DIR);
		
		if(System.getProperty(FITTESTConstants.FITTEST_ITE_HTTP_PORT)==null)
			System.setProperty(FITTESTConstants.FITTEST_ITE_HTTP_PORT, FITTESTConstants.DEFAULT_FITTEST_ITE_HTTP_PORT.toString());
		
		if(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_START)==null)
			System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_START, FITTESTConstants.DEFAULT_FITTEST_ITE_SERVER_PORT_START.toString());
		
		if(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_RANGE)==null)
			System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_RANGE, FITTESTConstants.DEFAULT_FITTEST_ITE_SERVER_PORT_RANGE.toString());	
		
		if(System.getProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR)==null)
			System.setProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR, FITTESTConstants.DEFAULT_FITTEST_SERVICE_FILETRANSFER_BASEDIR);
		else {
			System.setProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR, new File(System.getProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR)).toURI().toString());
		}
		Logger.getLogger("").setLevel(Level.INFO);
		
		new Main();
	}

	public synchronized void incomingEvent(ServiceEvent<?> event) {
		if(event instanceof RegistrationEvent){
			RegistrationEvent registrationEvent = (RegistrationEvent) event;
			for(String agent: registrationEvent.getSource().getAllAgents()){
				System.out.println("agent: "+agent);
				for(String component: registrationEvent.getSource().getAllComponentsOnAgent(agent)){
					System.out.println("\tcomponent: "+component);	
				}
			}
			switch (registrationEvent.getKind()) {
			case agentRegistration:
			case componentRegistration:				
				_model.addElement(registrationEvent.getRegistrationData().getEntity());
				break;
			case deregistration:
				_model.removeElement(registrationEvent.getRegistrationData().getEntity());
				break;
			default:
				break;
			}
			Logger.getAnonymousLogger().log(Level.INFO,registrationEvent.getKind().toString());
		}
		
	}
}
