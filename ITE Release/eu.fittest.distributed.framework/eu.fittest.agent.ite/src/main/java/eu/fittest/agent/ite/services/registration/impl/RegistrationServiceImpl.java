package eu.fittest.agent.ite.services.registration.impl;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Map.Entry;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.agent.ite.services.registration.spec.IRegistrationServiceListener;
import eu.fittest.agent.ite.services.registration.spec.RegistrationEvent;
import eu.fittest.agent.ite.services.registration.spec.RegistrationEventKind;
import eu.fittest.common.core.connection.spec.ConnectionServiceEvent;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.identity.spec.FITTESTAgent;
import eu.fittest.common.core.identity.spec.FITTESTComponent;
import eu.fittest.common.core.identity.spec.FITTESTEntity;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.common.services.registration.spec.RegistrationData;

public class RegistrationServiceImpl extends AbstractService implements
		IRegistrationService {
	private class CheckLease extends TimerTask {

		public void run() {
			ITELogger.log(Level.INFO, "checking lease...");
			synchronized(_routingTable){
				Set<RegistrationData> keys =  _routingTable.keySet();
				RegistrationData[] datas = keys.toArray(new RegistrationData[keys.size()]);
				for (RegistrationData data : datas) {					
					if (data.getLease() != 0
							&& System.currentTimeMillis() > data.getLease() * 1000) {
						ITELogger.log(Level.INFO, "deregistering due to expried lease: " + data.getEntity().getId());
						deregister(data.getEntity().getId());
					}
				}
			}

		}
	}

	private volatile Map<RegistrationData, String> _routingTable;
	private Timer _timer;

	public RegistrationServiceImpl() {
		super();
		_routingTable = new Hashtable<RegistrationData, String>();
		_handlers.add(new DeregisterMH(this));
		_handlers.add(new RegisterAgentMH(this));
		_handlers.add(new RegisterComponentMH(this));
		_handlers.add(new RegisteredComponentsMH(this));
		_timer = new Timer();
		_timer.schedule(new CheckLease(),
				FITTESTConstants.DEFAULT_CHECK_LEASE_PERIOD,
				FITTESTConstants.DEFAULT_CHECK_LEASE_PERIOD);

	}

	/**
	 * find Agent Registration data from the routing table
	 * 
	 *  @author cdnguyen
	 * @param publicIP
	 * @param publicPort
	 * @return
	 */
	private RegistrationData findAgentRegistrationData(String publicIP, int publicPort){
		RegistrationData data = null;

		// check if agent is already registered
		Iterator<Entry<RegistrationData, String>> it = _routingTable.entrySet()
				.iterator();
		while (data == null && it.hasNext()) {
			Entry<RegistrationData, String> entry = it.next();
			if (entry.getValue().equals(publicIP + ":" + publicPort)) {
				data = entry.getKey();
			}
		}
		return data;
	}
	
	
	/**
	 * Register a new agent or the agent renew its lease
	 * 
	 */
	public synchronized RegistrationData registerAgent(final String publicIP,
			final int publicPort, final HUTEnvironment environment,
			final HUTType type, String description, String oldId) {
		ITELogger.log(Level.INFO,
				"agent " + publicIP + ":" + publicPort + " is registered");

		RegistrationData data = findAgentRegistrationData(publicIP, publicPort);

		if (data != null) {// agent is already registered, renew lease
			data.setLease(System.currentTimeMillis() / 1000
					+ FITTESTConstants.REGISTRATION_LEASE_DURATION);
			fireEvent(new RegistrationEvent(this, data,
					RegistrationEventKind.leaseRenewal));
		} else {
			FITTESTAgent agent = null;
			if (oldId != null && isUnique(oldId)) {// check if agent has been
													// registered in the past
				agent = new FITTESTAgent(oldId, environment, type, description);// reusing
																				// same
																				// id
			} else {
				agent = new FITTESTAgent(environment, type, description);
			}
			data = new RegistrationData(System.currentTimeMillis() / 1000
					+ FITTESTConstants.REGISTRATION_LEASE_DURATION, agent,
					_registry.findService(IIdentityService.class)
							.getMyIdentity());
			_routingTable.put(data, publicIP + ":" + publicPort);
			_registry.findService(IConnectionService.class).map(
					agent.getId(),
					_registry.findService(IConnectionService.class)
							.getConnection(publicIP, publicPort));
			fireEvent(new RegistrationEvent(this, data,
					RegistrationEventKind.agentRegistration));
		}

		return data;
	}

	private boolean isUnique(String oldId) {
		boolean unique = true;
		synchronized (_routingTable) {
			Iterator<RegistrationData> it = _routingTable.keySet().iterator();
			while (unique && it.hasNext()) {
				unique = !it.next().getEntity().getId().equals(oldId);
			}
		}
		return unique;
	}

	/**
	 * Register a new component 
	 */
	public synchronized void registerComponent(final String agentID,
			final String componentID) {
		ITELogger.log(
				Level.INFO,
				"component " + componentID + " is registered on agent "
						+ agentID);
		
		RegistrationData agentData = getAgentData(agentID);
		
		FITTESTComponent component = new FITTESTComponent(componentID);
		if (agentData != null){
			if (agentData.getEntity() != null){
				component.setContainerAgent((FITTESTAgent) agentData.getEntity());
			}
		}
		
		RegistrationData componentData = new RegistrationData(0, component, _registry.findService(IIdentityService.class).getMyIdentity());
		_routingTable.put(componentData, agentID);

		String agentAddress = _routingTable.get(new RegistrationData(0,
				new FITTESTAgent(agentID), null));

		_registry.findService(IConnectionService.class).map(
				componentID,
				_registry.findService(IConnectionService.class).getConnection(
						agentAddress.split(":")[0],
						new Integer(agentAddress.split(":")[1])));
		fireEvent(new RegistrationEvent(this, componentData,
				RegistrationEventKind.componentRegistration));
	}

	
	/**
	 * Deregister and agent or component
	 */
	public void deregister(final String id) {
		
		removeRegistrationData(id);

		RegistrationData data = new RegistrationData(0, new FITTESTEntity(id),
				_registry.findService(IIdentityService.class).getMyIdentity());
		
		fireEvent(new RegistrationEvent(this, data,
				RegistrationEventKind.deregistration));

		// TODO: need to unmap connection ??? 
	}
	
	/**
	 * @author cdnguyen
	 * Remove registration data only, without propagating any event
	 */
	private void removeRegistrationData(final String id){
		synchronized (_routingTable){
			for (String component : getAllComponentsOnAgent(id)) {
				_routingTable.remove(new RegistrationData(0, new FITTESTComponent(
						component), null));
			}
	
			_routingTable.remove(new RegistrationData(0, new FITTESTEntity(id),
					null));// agent or component is removed
		}
	}

	public void addRegistrationListener(
			final IRegistrationServiceListener listener) {
		addServiceListener(listener);
	}

	public void removeRegistrationListener(
			final IRegistrationServiceListener listener) {
		removeServiceListener(listener);
	}

	public List<String> getAllAgents() {
		List<String> agents = new ArrayList<String>();
		synchronized (_routingTable) {
			for (Entry<RegistrationData, String> entry : _routingTable.entrySet()) {
				RegistrationData data = new RegistrationData(0, new FITTESTEntity(
						entry.getValue()), null);
				if (!_routingTable.containsKey(data)
						&& !agents.contains(entry.getValue())) {
					agents.add(entry.getKey().getEntity().getId());
				}
			}
		}
		return agents;
	}

	public List<String> getAllComponentsOnAgent(
			final String agentID) {
		List<String> components = new ArrayList<String>();
		synchronized(_routingTable){
			Set<RegistrationData> keys =  _routingTable.keySet();
			RegistrationData[] datas = keys.toArray(new RegistrationData[keys.size()]);
			for (RegistrationData key : datas) {
				if (_routingTable.get(key).equals(agentID))
					components.add(key.getEntity().getId());
			}
		}
		return components;
	}

	public String getName() {
		return IRegistrationService.class.getName();
	}

	public void incomingEvent(ConnectionServiceEvent event) {
		switch (event.getKind()) {
		case add:
			break;
		case remove:
			
			/*
			 * This step is still needed in case the connection is disconnected without deregistration 
			 */
			List<String> ids = event.getSource().getReachableId(
					event.getConnection());
			for (String id : ids) {
				deregister(id);
//				removeRegistrationData(id);
			}
			
			break;
		}
	}

	public RegistrationData getAgentData(String agentId) {
		RegistrationData data = null;
		synchronized (_routingTable) {
			Iterator<RegistrationData> it = _routingTable.keySet().iterator();
			while (data == null && it.hasNext()) {
				RegistrationData current = it.next();
				if (current.getEntity().getId().equals(agentId)) {
					data = current;
				}
			}
		}
		return data;
	}

}
