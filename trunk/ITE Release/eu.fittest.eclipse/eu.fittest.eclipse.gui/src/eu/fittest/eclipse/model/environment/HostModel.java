package eu.fittest.eclipse.model.environment;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.agent.ite.services.registration.spec.IRegistrationServiceListener;
import eu.fittest.agent.ite.services.registration.spec.RegistrationEvent;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.FITTESTAgent;
import eu.fittest.common.core.identity.spec.FITTESTComponent;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.util.ITELogger;
import eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager;
import eu.fittest.eclipse.startup.FITTESTServerStartUp;

public class HostModel implements IRegistrationServiceListener{
	private static HostModel _instance = null;
	private volatile List<Host> _hosts; // added the volatile keyword by Cu
	private List<IHostsListener> _hostsListeners; // by urueda
	private IConnectionService _connectionService;
	private List<IFITTESTComponentManager> _componentManagers;
	private IServiceRegistry _registry;
	
	private Map<String, List<String>> componentsAgentMap; // <agent, components> map to store which component is registered on which agent
	
	static public HostModel getInstance() throws FITTESTException{
		if(_instance == null){
			_instance = new HostModel();
		}
		return _instance;
	}
	
	private HostModel() throws FITTESTException{
		
		componentsAgentMap = new Hashtable<String, List<String>>();
		
//		_hosts = new ArrayList<Host>();
		_hosts = new CopyOnWriteArrayList<Host>(); // modified by Cu
		_hostsListeners = new CopyOnWriteArrayList<IHostsListener>(); // modified by Cu
		_registry = FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry();
		_connectionService = _registry.findService(IConnectionService.class);
		_componentManagers = new ArrayList<IFITTESTComponentManager>(); 
		
		findComponentExtensions();
		_connectionService.registerMessageHandler(new RegisteredComponentsMH(_registry.findService(IRegistrationService.class),_componentManagers));
	}
	
	private void createComponentExtensions(IConfigurationElement[] config){
		for (IConfigurationElement element : config) {
			try {
				Object o = element.createExecutableExtension("class");
				if (o instanceof IFITTESTComponentManager) {
					IFITTESTComponentManager cm = (IFITTESTComponentManager)o;
					cm.setServiceRegistry(_registry);
					_componentManagers.add(cm);
				}
			} catch (CoreException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getCause()+ ": "+ e.getMessage());
			}
			
		}
	}
	
	private void findComponentExtensions() {
		IConfigurationElement[] config = Platform.getExtensionRegistry().getConfigurationElementsFor(IFITTESTComponentManager.TESTING_SESSION_EXTENSION_POINT_ID);
		createComponentExtensions(config);		
	}

	/*@Override
	public void incomingEvent(RegistrationEvent event) {
		Host host = new Host();
		host.setName(event.getRegistrationData().getEntity().getId());
		host.setHUT(false);
		host.setDescription("");
		Connection c = _connectionService.getConnection(event.getRegistrationData().getEntity().getId());
		host.setIp(c.getRemoteAddress());
		switch (event.getKind()) {
			case agentRegistration:
				FITTESTAgent agent = (FITTESTAgent)event.getRegistrationData().getEntity();
				host.setEnvironment(agent.getEnvironment());
				host.setType(agent.getType());
				host.setDescription(agent.getDescription());
				_hosts.add(host);
				// by urueda
				if (_hostsListeners != null) {
					for (IHostsListener l : _hostsListeners) {
						l.notifyHostAdded(host);
					}
				}
				// end by urueda
				break;
			case deregistration:
				_hosts.remove(host);
				// by urueda
				if (_hostsListeners != null) {
					for (IHostsListener l : _hostsListeners) {
						l.notifyHostRemoved(host);
					}
				}
				// end by urueda
				break;
			case leaseRenewal:
				break;
			default:
				break;
		}
	}*/
	
	// by urueda, the synchnorized added keyword by Cu
	@Override
	public synchronized void incomingEvent(RegistrationEvent event) {
		switch (event.getKind()) {
			case agentRegistration:
				FITTESTAgent agent = (FITTESTAgent) event.getRegistrationData().getEntity();
				String regId = agent.getId();
				ITELogger.log(Level.INFO, "HostModel registering agent: " + regId); // for debug
				Host hmHost = getHost(regId);
				if (hmHost != null) {
					ITELogger.log(Level.INFO, "HostModel already contains agent: " + regId);
				}
				else {
					Host host = new Host();
					host.setName(regId);;
					host.setHUT(false);
					Connection c = _connectionService.getConnection(regId);
					host.setIp(c.getRemoteAddress());
					host.setEnvironment(agent.getEnvironment());
					host.setType(agent.getType());
					host.setDescription(agent.getDescription());
					
					// initiate the components map
					List<String> componentList = new ArrayList<String>();
					synchronized (componentsAgentMap){
						componentsAgentMap.put(regId, componentList);
					}

					// Log activity related to the host
					host.logActivity("ADDED");
					synchronized(_hosts){
						_hosts.add(host);
					}
					// by urueda
					if (_hostsListeners != null) {
						for (IHostsListener l : _hostsListeners) {
							l.notifyHostAdded(host);
						}
					}
					// end by urueda
				}
				break;
			case componentRegistration:
				FITTESTComponent component = (FITTESTComponent) event.getRegistrationData().getEntity();
				String componentId = component.getId();
				ITELogger.log(Level.INFO, "HostModel registering component: " + componentId);
				
				agent = component.getContainerAgent();
				if (agent != null) {
					Host host = getHost(agent.getId());
					if (host != null){
						
						host.logActivity("Component " + componentId  + " added");
						
						synchronized (componentsAgentMap){
							List<String> componentList = componentsAgentMap.get(agent.getId());
							if (componentList != null){
								componentList.add(componentId);
							}
						}
						
						if (_hostsListeners != null) {
							for (IHostsListener l : _hostsListeners) {
								l.notifyComponentAdded(host, componentId);
							}
						}
					} else {
						ITELogger.log(Level.WARNING, "HostModel: component " + componentId + " registerd before its agent " + agent.getId()) ;
					}
				} else {
					ITELogger.log(Level.WARNING, "HostModel: component " + componentId + " registerd on NULL agent?!") ;
				}
				
				break;
			case deregistration:
				// by urueda
				String deregId = event.getRegistrationData().getEntity().getId();
				ITELogger.log(Level.INFO,"HostModel deregistering host/component: " + deregId); // for debug
				Host deHost = getHost(deregId);
				if (deHost != null) {
					// Log activity related to the host
					deHost.logActivity("REMOVED");

					if (_hostsListeners != null) {
						for (IHostsListener l : _hostsListeners) {
							l.notifyHostRemoved(deHost);
						}
					}
					synchronized(_hosts){
						_hosts.remove(deHost);
					}
				} else {
					// remove a component 
					synchronized (componentsAgentMap){
						String agentId = null;
						for (String id : componentsAgentMap.keySet()){
							List<String> componentList = componentsAgentMap.get(id);
							if (componentList != null && componentList.contains(deregId)){
								agentId = id;
								break;
							}
						}
						
						if (agentId != null){
							List<String> componentList = componentsAgentMap.get(agentId);
							componentList.remove(deregId);
							deHost = getHost(agentId);
							if (deHost != null) {
								// Log activity related to the host
								deHost.logActivity("Component " + deregId  + " removed");
								if (_hostsListeners != null) {
									for (IHostsListener l : _hostsListeners) {
										l.notifyComponentRemoved(deHost, deregId);
									}
								}
							}
						}

					}
				}
				break;
			case leaseRenewal:
				break;
			default:
				break;
		}
	}

	public void addHostsListener(IHostsListener listener) {
		synchronized(_hostsListeners){
			if (!_hostsListeners.contains(listener))
				_hostsListeners.add(listener);
		}
	}
	
	public void removeHostsListener(IHostsListener listener) {
		synchronized(_hostsListeners){
			if (_hostsListeners.contains(listener))
				_hostsListeners.remove(listener);
		}
	}
	
	

	// by urueda
	private Host getHost(String id) {
		synchronized(_hosts){
			for (Host h : _hosts.toArray(new Host[_hosts.size()])) {
				if (h.getName().equals(id)) {
					return h;
				}
			}
		}
		return null;
	}
	
	
	public List<Host> getHosts(){
		synchronized(_hosts){
			return _hosts;
		}
	}
}
