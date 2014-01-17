package eu.fittest.eclipse.model.jobs;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Timer; 
import java.util.TimerTask; 
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;

import eu.fittest.common.core.constants.FITTESTConstants; 
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.core.xml.HUTType; 
import eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.environment.HostStates;
import eu.fittest.eclipse.startup.FITTESTServerStartUp;

public abstract class HUTManager{
	
	private static final String LOGGER_PREFIX = "eu.fittest.Logging::FittestLogger-";
	
	
	/**
	 * Single upload thread for all hosts, call for upload as soon as 
	 * a host is at state UPLOADING_STARTED
	 * @author cdnguyen
	 *
	 */
	public class UploadTask extends TimerTask{
		@Override
		public void run() {
			Host[] hosts = hutList.toArray(new Host[hutList.size()]);
			for (Host h : hosts){
				if (h.getCurrentLifeState().equals(HostStates.UPLOADING_STARTED)) {
					uploadHUT(h);
				} else if (h.getCurrentLifeState().equals(HostStates.UPLOADING_FAILED)){
					Vector<String> lifeStates = h.getLifeStates();
					if (lifeStates.size() > 3 &&
							lifeStates.get(1).equals(HostStates.UPLOADING_FAILED) &&
							lifeStates.get(2).equals(HostStates.UPLOADING_FAILED)){
						// Failed three times, stop dealing with this host
						h.setCurrentLifeState(HostStates.UPLOADING_FINISHED);
					} else {
						// try a gain
						uploadHUT(h);
					}
				}
			}
		}
	}
		
	protected Timer uploadsTimer; // a single upload timer for all hosts
	
	private final long upPeriod = FITTESTConstants.FITTEST_ASYNCHRONOUS_UPLOADS_INTERVAL; 

	protected IFolder session;
	protected SessionType sessionType;
	
	protected IServiceRegistry registry;

	private List<IFITTESTComponentManager> componentManagers;
	
	private List<Host> hutList;
	private Map<Host, List<String>> componentsHostMap;
	
	public HUTManager(IFolder session, List<IFITTESTComponentManager> componentManagers, List<Host> huts, SessionType sessionType) throws FITTESTException {
		this.session = session;
		this.sessionType = sessionType;
		
		this.registry = FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry();
		this.hutList = huts;
		this.componentManagers = componentManagers;
		for(IFITTESTComponentManager manager: componentManagers){
			manager.setServiceRegistry(registry);
		}
		deployComponents();
		for(IFITTESTComponentManager manager: componentManagers){
			manager.prepare(session, sessionType);
		}
		
		
		componentsHostMap = new Hashtable<Host, List<String>>();
		for (Host h : huts){
			componentsHostMap.put(h, new ArrayList<String>());
		}
	}
	
	public void addHUT(Host h) {
		synchronized(hutList){
			if (hutList != null) {
				hutList.add(h);
				
				synchronized (componentsHostMap){
					componentsHostMap.put(h, new ArrayList<String>());
				}
				
				try {
					deployComponents(h);
				}
				catch (FITTESTException fe) {
					Logger.getAnonymousLogger().log(Level.WARNING, "Exception deploying host components: " + h.toString());
				}
			}
		}
	}

	public void removeHUT(Host h) {
		synchronized (hutList){
			if (hutList != null) {
				stopUploads(h);
				hutList.remove(h);
				
				synchronized (componentsHostMap){
					componentsHostMap.remove(h);
				}
			}
		}
	}
	
	
	public void addComponentOnHost(String componentId, Host h){
		synchronized(hutList){
			if (hutList != null && hutList.contains(h)) {
				synchronized (componentsHostMap){
					List<String> componentList = componentsHostMap.get(h);
					if (!componentList.contains(componentId)){
						componentList.add(componentId);
						if (componentId.startsWith(LOGGER_PREFIX)){
							startUploads(h);
						}
					}
					
				}
			}
		}
	}
	
	
	public void removeComponentOnHost(String componentId, Host h){
		synchronized(hutList){
			if (hutList != null && hutList.contains(h)) {
				synchronized (componentsHostMap){
					List<String> componentList = componentsHostMap.get(h);
					if (componentList.contains(componentId))
						componentList.remove(componentId);
					
					if (componentId.startsWith(LOGGER_PREFIX)){
						stopUploads(h);
					}
				}
			}
		}
	}
	

	protected void deployComponents() throws FITTESTException {
		synchronized (hutList){
			for(IFITTESTComponentManager cm: componentManagers){
				for(Host h: hutList){
					cm.deployOn(h);
				}
			}
		}
	}
	
	protected void deployComponents(Host h) throws FITTESTException {
		h.logActivity("Call deploy component");
		for(IFITTESTComponentManager cm: componentManagers) {
			cm.deployOn(h);
		}
		// set current state of the host
		h.setCurrentLifeState(HostStates.COMPONENT_READY);
	}

	public void initializeHUTs() throws FITTESTException {
		synchronized (hutList){
			for(IFITTESTComponentManager cm: componentManagers){
				for(Host h: hutList){
					cm.initialize(h);
				}
			}
		}
	}

	public void initializeHUT(Host h) throws FITTESTException {	
		for(IFITTESTComponentManager cm: componentManagers){
			cm.initialize(h);
		}
		h.logActivity("Initialized");
		// set current state of the host
		h.setCurrentLifeState(HostStates.INITIALIZED);
	}

	private void startUploads(Host h) {
		if (h.getType() != HUTType.SERVER) {
			h.setCurrentLifeState(HostStates.UPLOADING_STARTED);
			h.logActivity("Start upload");
			
			for(IFITTESTComponentManager cm: componentManagers){
				try {
					cm.beginUploadSession(h);
				}
				catch (FITTESTException fe) {
					Logger.getAnonymousLogger().log(Level.WARNING, "Failed to begin uploading");
				}
			}
		}
	}
	
	private void stopUploads(Host h) {
		if (h.getType() != HUTType.SERVER) {
			h.setCurrentLifeState(HostStates.UPLOADING_FINISHED);
			h.logActivity("Stop upload");
			
			for(IFITTESTComponentManager cm: componentManagers){
				try {
					cm.endUploadSession(h);
				}
				catch (FITTESTException fe) {
					Logger.getAnonymousLogger().log(Level.WARNING, "Failed to begin uploading");
				}
			}
		}
	}

	public void uploadHUT(Host h) {
		if (!shouldUpload(h))
			return; // do nothing 
		
		h.logActivity("Call upload");
		for(IFITTESTComponentManager cm: componentManagers){
			try {
				cm.upload(h);
			}
			catch (FITTESTException fe) {
				Logger.getAnonymousLogger().log(Level.WARNING, "FITTEST exception on periodic log uploads");
			}
		}
	}
	
	/**
	 * Check whether the host should be called for uploading
	 * 
	 * @param h
	 * @return
	 */
	private boolean shouldUpload(Host h){
		synchronized (componentsHostMap){
			List<String> components = componentsHostMap.get(h);
			if (components != null){
				for (String componentId : components){
					if (componentId.startsWith(LOGGER_PREFIX)){
						return true;
					}
				}
			}
		}
		
		return false;
	}
	
	public void startHUTs() throws FITTESTException{
		synchronized(hutList){
			for(Host h: hutList){
				startHUT(h);
			}
		}
		// start the timer for geting log 
		uploadsTimer = new Timer("Timer for getting logs");
		TimerTask tt = new UploadTask();
		uploadsTimer.scheduleAtFixedRate(tt, upPeriod, upPeriod);
		
	}
	
	public void startHUT(Host h) throws FITTESTException{
		h.logActivity("Call start");
		for(IFITTESTComponentManager cm: componentManagers){
			cm.start(h);
		}
		startUploads(h);
	}

	public void stopHUTs() throws FITTESTException{
		synchronized(hutList){
			for(IFITTESTComponentManager cm: componentManagers){
				for(Host h: hutList){
					stopUploads(h); 
					cm.stop(h);
				}
			}
		}
		
		uploadsTimer.cancel();
	}
	
	public void terminateHUTs() throws FITTESTException{
		synchronized(hutList){
			for(IFITTESTComponentManager cm: componentManagers){
				for(Host h: hutList){
					cm.terminate(h);
				}
				
				// call post process methods
				cm.postProcess(session, sessionType);
			}
		}
	}
}
