package eu.fittest.component.optimizer;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import ocon.entry.OptimizeKingProperties;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTConnectionClosedException;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.exception.FITTESTExceptionListener;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.IServiceListener;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.Initialize;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.common.core.xml.Progress;
import eu.fittest.common.core.xml.Start;
import eu.fittest.common.core.xml.Stop;
import eu.fittest.common.core.xml.Terminate;
import eu.fittest.common.core.xml.UploadResource;
import eu.fittest.common.services.compression.impl.CompressionServiceImpl;
import eu.fittest.common.services.filetransfer.impl.FileTransferServiceImpl;
import eu.fittest.common.services.filetransfer.spec.FileEvent;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.common.util.FileUtils;
import eu.fittest.component.common.AbstractComponent;
import eu.fittest.component.junit.ite.services.testcases.impl.TestCasesServiceImpl;
import eu.fittest.component.junit.ite.services.testcases.spec.ITestCasesService;
import eu.fittest.component.junit.ite.services.testcases.spec.TestCasesEvent;
import eu.fittest.component.services.registration.spec.IComponentRegistrationService;

public class ConTestOptimizerComponent extends AbstractComponent implements FITTESTExceptionListener, IOptimizerRequiredServices, IServiceListener<ServiceEvent<?>>{
	private String _contestComponentId;
	private String _junitComponentId;
	private String _sessionName;
	private String _jarFile;
	private IOptimizerProvidedServices _optimizer;
	private IConnectionService _connectionService;
	private int _totalNbTestCases = 0;
	
	private boolean _completed = false;
	
	protected ConTestOptimizerComponent(int port) throws FITTESTException {
		super(ConTestOptimizerComponent.class.getSimpleName(), port);
		_serviceRegistry.registerService(new FileTransferServiceImpl());
		_serviceRegistry.registerService(new CompressionServiceImpl());
		_serviceRegistry.registerService(new TestCasesServiceImpl());
		_connectionService = _serviceRegistry.findService(IConnectionService.class);
	}
	
	public void registered() {
		try {
			_serviceRegistry.findService(IFileTransferService.class).setBasePath(_serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir());
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE,e.getMessage());
		}
	}
	
	@Override
	public void register() throws FITTESTException {
		super.register();
		_connection.addFITTESTExceptionListener(this);
	}
	
	@Override
	public void uncaughtException(Throwable t) {
		if(t instanceof FITTESTConnectionClosedException){
			Logger.getAnonymousLogger().log(Level.INFO, "Connection to agent closed, exiting");
			stop();
			FITTESTSingleton.shutdown();
		}
	}

	public void initialize(List<Parameter> parameters) {
		_completed = false;
		_optimizer = new OptimizeKingProperties();
		_optimizer.setOptimizerRequiredServices(this);

		Properties properties = new Properties();
		
		for(Parameter p: parameters){
			if(p.getName().equals("session.name")){
				_sessionName = p.getValue();
			}
			else if(p.getName().equals("targetClasses")){
				properties.put(IOptimizerProvidedServices.TARGET_CLASSES, p.getValue());
			}
			else if(p.getName().equals("java.executablejar")){
				_jarFile = p.getValue();
			}
			else if(p.getName().equals("junitcomponent.id")){
				_junitComponentId = p.getValue();
			}
			else if(p.getName().equals("contestcomponent.id")){
				_contestComponentId = p.getValue();
			}
			else if(p.getName().equals(IOptimizerProvidedServices.SEARCH_ALGORITHM)){
				_optimizer.setSearchAlgorithm(SearchAlgorithm.valueOf(p.getValue()));
			}
			else if(p.getName().equals(IOptimizerProvidedServices.RANDOM_NUMBER_SEED)){
				Long seed = Long.valueOf(p.getValue());
				if(seed!=-1L) _optimizer.setRandomNumberSeed(seed);
			}
		}
		
		_optimizer.setSeedProperties(properties);
		
		try {
			File outputFolder = new File(new URI(_serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir()+_sessionName));
			if(!outputFolder.exists() && !outputFolder.mkdirs()){
				Logger.getAnonymousLogger().log(Level.SEVERE, "can't create output folder "+ outputFolder+" for optimizer");
			}
			_optimizer.setOutputFolder(outputFolder);
			
		} catch (URISyntaxException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}

	public void start() {
		FITTESTSingleton.getThreadPool().execute(new Runnable() {		
			@Override
			public void run() {
				_optimizer.run();
			}
		});
	}

	public synchronized void stop() {
		if(_optimizer!=null) _optimizer.interrupt();
	}

	private class TerminateTask implements Runnable{
		
		private ConTestOptimizerComponent _component;

		public TerminateTask(ConTestOptimizerComponent component){
			_component = component;
		}

		@Override
		public void run() {
			synchronized (_component) {
				while(!_completed){
					try {
						_component.wait();
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
			}

			Terminate terminateJUnit = FITTESTSingleton.getObjectFactory().createTerminate();
			String myId= _serviceRegistry.findService(IIdentityService.class).getMyIdentity();
			terminateJUnit.setFrom(myId);
			terminateJUnit.setTo(_junitComponentId);
			
			UploadResource upload = FITTESTSingleton.getObjectFactory().createUploadResource();
			upload.setFrom(_serviceRegistry.findService(IIdentityService.class).getMyIdentity());
			upload.setTo(_serviceRegistry.findService(IComponentRegistrationService.class).getAgentId());
			upload.setTowards(_serviceRegistry.findService(IComponentRegistrationService.class).getIteId());
			upload.setResource(_serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir()+_sessionName+"/KingProperties");
			try {
				_connectionService.sendMessage(terminateJUnit);
				_connectionService.sendMessage(upload);
			} catch (FITTESTException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}
		
	}
	
	public synchronized void terminate() {
		//send King properties to ITE
		FITTESTSingleton.getThreadPool().execute(new TerminateTask(this));
	}
	
	private Parameter createParameter(String name, String value){
		Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
		p.setName(name);
		p.setValue(value);
		return p;
	}
	
	
	/** start a Java application instrumented with ConTest configured with the given set of King properties and executes a set of test cases
	 * When the time limit is reached or when test cases execution is completed, it returns the path to the folder containing the log files
	 * (e.g. /tmp/sessionid/com_ibm_contest/)
	 * 
	 * @param kingProperties: set of King properties whose name are the same as the one specified in the KingProperties file
	 * @param timeLimit: maximum duration of the test case execution
	 * @return folder containing the contest log files (e.g. com_ibm_contest)
	 * @throws FITTESTException 
	 */
	@Override
	public synchronized File execute(Properties kingProperties) throws FITTESTException {
		File resultFolder;
		try {
			resultFolder = new File(new URI(_serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir()+ "com_ibm_contest"));
		} catch (URISyntaxException e2) {
			throw new FITTESTException(e2.getMessage());
		}
		if(resultFolder.exists()){
			if(!FileUtils.deleteDir(resultFolder)){
				throw new FITTESTException(resultFolder+ " can't be deleted");
			}
		}
		
		String myId= _serviceRegistry.findService(IIdentityService.class).getMyIdentity();
		
		Initialize initConTest = FITTESTSingleton.getObjectFactory().createInitialize();
		initConTest.setFrom(myId);
		initConTest.setTo(_contestComponentId);
		
		for(Object o : kingProperties.keySet()){
			initConTest.getParameter().add(createParameter(o.toString(), kingProperties.get(o).toString()));	
		}
		initConTest.getParameter().add(createParameter("results.uploadTowards", myId));
		initConTest.getParameter().add(createParameter("session.name", _sessionName));
		initConTest.getParameter().add(createParameter("java.executablejar", _jarFile));
		
		_connectionService.sendMessage(initConTest);
		
		Start startConTest = FITTESTSingleton.getObjectFactory().createStart();
		startConTest.setFrom(myId);
		startConTest.setTo(_contestComponentId);
		_connectionService.sendMessage(startConTest);
		
		_serviceRegistry.findService(ITestCasesService.class).addServiceListener(this);
		Start startJUnit = FITTESTSingleton.getObjectFactory().createStart();
		startJUnit.setFrom(myId);
		startJUnit.setTo(_junitComponentId);
		_connectionService.sendMessage(startJUnit);
	
		try {
			wait();
		} catch (InterruptedException e1) {
			e1.printStackTrace();
		}
		_serviceRegistry.findService(ITestCasesService.class).removeServiceListener(this);
		
		Stop stopJUnit = FITTESTSingleton.getObjectFactory().createStop();
		stopJUnit.setFrom(myId);
		stopJUnit.setTo(_junitComponentId);
		_connectionService.sendMessage(stopJUnit);
		
		Stop stopConTest = FITTESTSingleton.getObjectFactory().createStop();
		stopConTest.setFrom(myId);
		stopConTest.setTo(_contestComponentId);
		_connectionService.sendMessage(stopConTest);
		
		_serviceRegistry.findService(IFileTransferService.class).addServiceListener(this);
		
		Terminate terminateConTest = FITTESTSingleton.getObjectFactory().createTerminate();
		terminateConTest.setFrom(myId);
		terminateConTest.setTo(_contestComponentId);
		_connectionService.sendMessage(terminateConTest);
		
		try {
			wait();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		_serviceRegistry.findService(IFileTransferService.class).removeServiceListener(this);
		
		if(!resultFolder.exists()){
			throw new FITTESTException(resultFolder+ " has not been uploaded");
		}
			
		return resultFolder;
	}
	
	@Override
	public synchronized void incomingEvent(ServiceEvent<?> event) {
		if(event instanceof FileEvent){
			FileEvent fevent = (FileEvent) event;
			switch(fevent.getKind()){
			case upload:
				Logger.getAnonymousLogger().log(Level.INFO,fevent.getKind().toString()+": "+fevent.getPath());
				notifyAll();
				break;
			default:
				break;
			}
		}
		else if(event instanceof TestCasesEvent){
			TestCasesEvent tcevent = (TestCasesEvent) event;
			Logger.getAnonymousLogger().log(Level.INFO,"Optimizer tc event: " +tcevent.getTestCaseName()+" "+tcevent.getTestCaseNumber());
			if(tcevent.getTestCaseName() == null && _totalNbTestCases ==0){//no name so starting the suite
				_totalNbTestCases = tcevent.getTestCaseNumber();
			}
			else{
				switch (tcevent.getStatus()) {
				case FINISHED:
					_totalNbTestCases--;
					break;
				case STARTED:
					break;
				default:
					break;
				}
				if(_totalNbTestCases==0){
					notifyAll();
				}
			}
		}
	}

	@Override
	public synchronized void setProgress(int value) {
		Logger.getAnonymousLogger().log(Level.INFO, "Optimizer progression: "+value+"/100");
		Progress progressMessage = FITTESTSingleton.getObjectFactory().createProgress();
		progressMessage.setProgress(value);
		progressMessage.setFrom(_serviceRegistry.findService(IIdentityService.class).getMyIdentity());
		progressMessage.setTo(_serviceRegistry.findService(IComponentRegistrationService.class).getIteId());
		progressMessage.setTaskId(progressMessage.getFrom());
		try {
			_connectionService.sendMessage(progressMessage);
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.INFO, e.getMessage());
		}
		
		if(value >= 100){
			_completed = true;
			notifyAll();
		}
	}
	
	@Override
	public synchronized void resourceAvailable(String comingFrom, String resource) {
		if(comingFrom.equals(_contestComponentId)){
			notifyAll();
		}
	}

}
