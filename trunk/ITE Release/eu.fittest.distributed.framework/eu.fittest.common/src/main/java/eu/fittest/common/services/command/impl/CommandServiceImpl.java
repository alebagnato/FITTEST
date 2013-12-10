package eu.fittest.common.services.command.impl;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Vector;
import java.util.logging.Level;

import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.services.command.spec.ICommandService;
import eu.fittest.common.services.command.spec.ICommandServiceListener;


public class CommandServiceImpl extends AbstractService implements ICommandService {
    
    private Map<String , Process> _processes;


    
    
    public String getName() {
        return ICommandService.class.getName();
    }

    
    
    public synchronized void addCommandServiceListener(final ICommandServiceListener listener) {
        super.addServiceListener(listener);
    }

    
    
    public synchronized void removeCommandServiceListener(final ICommandServiceListener listener) {
        super.removeServiceListener(listener);
    }

    
    
    public synchronized String execute(final String command, final Map<String, String> parameters, final Map<String, String> environment, final URI workingDirFileURI) throws FITTESTException {
        try {
        	String[] env = null;
        	if(environment!=null){
	        	Vector<String> envi = new Vector<String>();
	        	for(Entry<String,String> p : environment.entrySet()){
	        		envi.add(p.getKey()+"="+p.getValue());
	        	}
	        	env = envi.toArray(new String[]{});
        	}
        	
        	Vector<String> params = new Vector<String>();
        	Collections.addAll(params, command.split(" "));
        	if(parameters!=null){        	
	        	for(Entry<String,String> p : parameters.entrySet()){
	        		params.add(p.getKey()+" "+p.getValue());
	        	}
        	}
        	String[] commandLine = params.toArray(new String[]{});
        	
        	File workDir = null;
        	if(workingDirFileURI==null){
        		workDir = new File(new URI(System.getProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR)+
        				_registry.findService(IIdentityService.class).getMyIdentity()));
        	}
        	else{
        		workDir = new File(workingDirFileURI);
        	}
        	if(!workDir.exists()) workDir.mkdirs();
        	
            Process process = Runtime.getRuntime().exec(commandLine,env, workDir);
            
            FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getErrorStream(), Level.SEVERE));
            FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getInputStream(), Level.INFO));
            String id = new Integer(process.hashCode()).toString();
            _processes.put(id, process);
            return id;
        } catch (IOException e) {
            throw new FITTESTException(e.getMessage());
        } catch (URISyntaxException e) {
			throw new FITTESTException(e.getMessage());
		}
    }

    
    public CommandServiceImpl() {
        super();
        assert(System.getProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR)!=null);
        _processes = new HashMap<String, Process>();
        _handlers.add(new AbortMH(this));
        _handlers.add(new ExecuteMH(this));
    }

    
    
    public synchronized void kill(final String taskID) throws FITTESTException {
    	Process p = _processes.remove(taskID);
        p.destroy();
        try {
			p.waitFor();
		} catch (InterruptedException e) {
			throw new FITTESTException(e.getMessage());
		}
    }

	
	public synchronized void killAllRunningTasks() throws FITTESTException {
		Vector<String> tasks = new Vector<String>();
		for(String key:	_processes.keySet()){
			tasks.add(key);
		}
		for(String taskId : tasks){
			kill(taskId);
		}
	}

}
