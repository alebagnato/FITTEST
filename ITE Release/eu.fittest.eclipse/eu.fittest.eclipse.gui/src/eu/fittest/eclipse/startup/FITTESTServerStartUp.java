package eu.fittest.eclipse.startup;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.ui.IStartup;

import eu.fittest.agent.ite.core.ITEAgent;
import eu.fittest.agent.ite.services.progress.impl.ProgressServiceImpl;
import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.agent.sut.ui.controller.AgentGUIController;
import eu.fittest.agent.sut.ui.gui.SUTAgentFrame;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.component.junit.ite.services.testcases.impl.TestCasesServiceImpl;
import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.model.environment.HostModel;

public class FITTESTServerStartUp implements IStartup{

	private static ITEAgent _agent = null;
	private static AgentGUIController _sutAgent = null;
	
	public static void exitAgentGUIController(){
		if(_sutAgent!=null && !_sutAgent.isTerminated()){
			_sutAgent.actionPerformed(new ActionEvent(_sutAgent, 0, "exit"));
		}
	}
	
	public static ITEAgent getITEAgentInstance() throws FITTESTException{
		if(_agent==null){
			configureProperties();
			_agent = new ITEAgent();
			_agent.getServiceRegistry().findService(IRegistrationService.class).addServiceListener(HostModel.getInstance());
			_agent.getServiceRegistry().registerService(new TestCasesServiceImpl());
			_agent.getServiceRegistry().registerService(new ProgressServiceImpl());
			_agent.start();
		}
		return _agent;
	}
	
	static public void loadLocalSUTAgent() throws FITTESTException{
		boolean enabled = Activator.getDefault().getPreferenceStore().getBoolean("ite.sut.agent.enabled");
		if(enabled && (_sutAgent == null || _sutAgent.isTerminated())){
			SUTAgentFrame frame = new SUTAgentFrame();
			frame.setDescription("This is the local agent executed by the Eclipse FITTEST ITE");
			frame.setHUTEnvironment(HUTEnvironment.TEST);
			frame.setHUTType(HUTType.MIXED);
			_sutAgent = new AgentGUIController(frame);
			_sutAgent.actionPerformed(new ActionEvent(_sutAgent, 0, "register"));
		}
		else if(!enabled && _sutAgent!=null && !_sutAgent.isTerminated()){
			_sutAgent.actionPerformed(new ActionEvent(_sutAgent, 0, "exit"));
		}
	}
	
	/**
	 * Shutdown the local SUT Mixed agent if started
	 */
	public static void shutdownLocalSUTAgent(){
		if(_sutAgent!=null && !_sutAgent.isTerminated()){
			_sutAgent.actionPerformed(new ActionEvent(_sutAgent, 0, "exit"));
		}
	}

	static synchronized private void configureProperties(){
			if(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_JNLP_FILENAME)==null)
				System.setProperty(FITTESTConstants.FITTEST_SUT_AGENT_JNLP_FILENAME, FITTESTConstants.DEFAULT_FITTEST_SUT_AGENT_JNLP_FILENAME);	
			
			if(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_JAR_FILENAME)==null)
				System.setProperty(FITTESTConstants.FITTEST_SUT_AGENT_JAR_FILENAME, "eu.fittest.agent.sut.ui.jar"+File.pathSeparator+"eu.fittest.agent.sut.jar"+File.pathSeparator+"eu.fittest.common.jar");		
		
			if(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_MAINCLASS_NAME)==null)
				System.setProperty(FITTESTConstants.FITTEST_SUT_AGENT_MAINCLASS_NAME, "eu.fittest.agent.sut.Main");			
			
			if(System.getProperty(FITTESTConstants.FITTEST_ITE_HTTP_DIR)==null){
				String path;
				try {
					path = FileLocator.toFileURL(new URL("platform:/plugin/"+Activator.PLUGIN_ID+"/"+FITTESTConstants.DEFAULT_FITTEST_ITE_HTTP_DIR)).getPath();
					System.setProperty(FITTESTConstants.FITTEST_ITE_HTTP_DIR, path);
				} catch (MalformedURLException e) {
					e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			
			if(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_AUTOREGISTRATION_DELAY)==null)
				System.setProperty(FITTESTConstants.FITTEST_SUT_AGENT_AUTOREGISTRATION_DELAY, Long.toString(FITTESTConstants.DEFAULT_AUTOREGISTRATION_DELAY));
			
			if(System.getProperty(FITTESTConstants.FITTEST_ITE_HTTP_PORT)==null)
				System.setProperty(FITTESTConstants.FITTEST_ITE_HTTP_PORT, FITTESTConstants.DEFAULT_FITTEST_ITE_HTTP_PORT.toString());

			if(System.getProperty(FITTESTConstants.FITTEST_ITE_ADDRESS)==null)
				System.setProperty(FITTESTConstants.FITTEST_ITE_ADDRESS, "127.0.0.1");

			if(System.getProperty(FITTESTConstants.FITTEST_ITE_PORT)==null)
				System.setProperty(FITTESTConstants.FITTEST_ITE_PORT, Integer.toString(37500));
			
			if(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_START)==null)
				System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_START, Integer.toString(37500));
			
			if(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_RANGE)==null)
				System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_RANGE, FITTESTConstants.DEFAULT_FITTEST_ITE_SERVER_PORT_RANGE.toString());	

			if(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_INCREMENT)==null)
				System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_INCREMENT, FITTESTConstants.DEFAULT_FITTEST_SERVER_PORT_INCREMENT.toString());
			
			if(System.getProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR)==null)
				System.setProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR, FITTESTConstants.DEFAULT_FITTEST_SERVICE_FILETRANSFER_BASEDIR);
			else {
				System.setProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR, new File(System.getProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR)).toURI().toString());
			}
			
	}
	
	@Override
	public void earlyStartup() {		
			try {
				getITEAgentInstance();
				loadLocalSUTAgent();
			} catch (FITTESTException e) {
				e.printStackTrace();
			}
	}
	
	

}
