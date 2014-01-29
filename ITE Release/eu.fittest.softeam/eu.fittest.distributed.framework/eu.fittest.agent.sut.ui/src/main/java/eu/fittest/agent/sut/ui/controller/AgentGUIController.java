package eu.fittest.agent.sut.ui.controller;

import java.awt.AWTEvent;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;

import javax.swing.JOptionPane;

import eu.fittest.common.util.ITELogger;
import eu.fittest.agent.sut.AppLock;
import eu.fittest.agent.sut.core.SUTAgent;
import eu.fittest.agent.sut.services.component.spec.IComponentRegistrationService;
import eu.fittest.agent.sut.services.ite.spec.IITERegistrationService;
import eu.fittest.agent.sut.services.serialization.spec.ISerializationService;
import eu.fittest.agent.sut.ui.IAgentView;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.common.services.forward.spec.IForwardService;

public class AgentGUIController implements ActionListener{
	private IAgentView _view;
	private SUTAgent _agent;
	private boolean isRegistered = false; 
	private boolean isRegistering = false; 
	
	public boolean isRegistered() {
		return isRegistered;
	}


	public AgentGUIController(IAgentView view) {
		_view = view;
		_view.addExitActionListener(this);
		_view.addRegisterActionListener(this);
	}
	

	private void startAgent() throws FITTESTException {
		_agent = new SUTAgent();
		
		_agent.getServiceRegistry().findService(IComponentRegistrationService.class).addServiceListener(_view);
		_agent.getServiceRegistry().findService(IITERegistrationService.class).addServiceListener(_view);
		_agent.getServiceRegistry().findService(ISerializationService.class).addServiceListener(_view);
		_agent.getServiceRegistry().findService(IForwardService.class).addServiceListener(_view);
		
		_agent.start();
	}
	
	public void open(){
		_view.open();
	}
	
	public boolean isTerminated(){
		return (_view == null);
	}

	@Override
	public synchronized void actionPerformed(ActionEvent event) {
		if(event.getActionCommand().equals("exit")){
			exit();
		}
		else if(event.getActionCommand().equals("register")){
			if (!isRegistering){
				isRegistering = true;
				register();
			}
		}		
	}
	
	private void exit(){
		ITELogger.log(Level.INFO, "Exiting FITTEST SUT Agent");
		deregister();
		if (_view != null){
			_view.close();
			_view.exit();
			_view = null;
		}
		
		// Exit only if the agent is not attached to Eclipse, assuming that 
		// the mixed agent is started only locally with Eclipse
		if (_agent.getType() != HUTType.MIXED)
			System.exit(0);
	}
	
	public void deregister(){
		if (isRegistered){
			_agent.getServiceRegistry().findService(IComponentRegistrationService.class).removeServiceListener(_view);
			_agent.getServiceRegistry().findService(IITERegistrationService.class).removeServiceListener(_view);
			_agent.getServiceRegistry().findService(ISerializationService.class).removeServiceListener(_view);
			_agent.getServiceRegistry().findService(IForwardService.class).removeServiceListener(_view);
			
			try {
				_agent.deregister();
//				try {
//					// wait for deregister message to be delivered
//					Thread.sleep(500);
//				} catch (InterruptedException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				} 
				if (_agent != null)
					_agent.stop();
				isRegistered = false;
				AppLock.releaseLock();
			} catch (FITTESTException e) {
				_view.displayMessage(Level.SEVERE, e.getMessage());
			}
		}
	}
	

	private synchronized void register(){
		
		try {
//			if (AppLock.setLock("fittest.agent.SUTAgent." + _view.getHUTType())) {

				ITELogger.log(Level.INFO, "Starting SUT Agent: " + _view.getHUTType());
				startAgent();
				isRegistered = false;
				ExecutorService executor = Executors.newSingleThreadExecutor();
		        Future<String> future = executor.submit(new Callable<String>(){

					@Override
					public String call() throws Exception {
						_agent.register(_view.getHUTEnvironment(), _view.getHUTType(),
								_view.getDescription());
						return "done";
					}
		        	
		        });

		        try {
		        	ITELogger.log(Level.INFO, "Registering FITTEST SUT Agent with ITE");
		            String result = future.get(7, TimeUnit.SECONDS); // 7 seconds 
		            if ("done".equals(result)){
		            	isRegistered = true;
						_view.close();
		            }
		            executor.shutdownNow();
		        } catch (TimeoutException e) {
		            System.out.println("Terminated!");
		            
		        } catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (ExecutionException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

				if (!isRegistered){
					JOptionPane
					.showMessageDialog(new Frame(),
							"Ops, something went wrong while registering with the ITE!");
					exit();
					System.exit(0);
				}
				
//			} else {
//				JOptionPane.showMessageDialog(new Frame(),
//						"There is an FITTEST agent (" + _view.getHUTType() + ") running on your system, please close it first!");
////							"Sorry, only one instance of the agent " + _view.getHUTType() + " can run at a time!");
//				exit();
//				System.exit(0);
//			}
		} catch (FITTESTException e) {
			_view.displayMessage(Level.SEVERE, e.getMessage());
		}
	}
}
