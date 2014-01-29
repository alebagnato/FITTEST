package eu.fittest.eclipse.component.extensionpoint;

import org.eclipse.core.resources.IFolder;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.SessionType;
import eu.fittest.project.config.SUTTechnologyType;

public interface IFITTESTComponentManager {
	String TESTING_SESSION_EXTENSION_POINT_ID = "eu.fittest.eclipse.gui.wizard.session.parameters";
	
	public void setServiceRegistry(IServiceRegistry registry);
	
	public void deployOn(Host host)  throws FITTESTException;
	
	public IFITTESTComponentWizardPage[] getConfigurationPages();
	
	public SUTTechnologyType[] getSupportSUT();
	
	public void prepare(IFolder sessionFolder, SessionType type) throws FITTESTException;
	
	public void initialize(Host host) throws FITTESTException;
	
	public void start(Host host) throws FITTESTException;
	
	public void upload(Host host) throws FITTESTException;
	
	/**
	 * Call before calling periodically the upload method 
	 * @param host
	 * @throws FITTESTException
	 */
	public void beginUploadSession(Host host) throws FITTESTException;
	
	/**
	 * Call after calling periodically the upload method 
	 * @param host
	 * @throws FITTESTException
	 */
	public void endUploadSession(Host host) throws FITTESTException; 
	
	public void stop(Host host) throws FITTESTException;
	
	public void terminate(Host host) throws FITTESTException;
	
	public void postProcess(IFolder sessionFolder, SessionType type);
}
