package eu.fittest.component.junit;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openqa.selenium.server.RemoteControlConfiguration;
import org.openqa.selenium.server.SeleniumServer;

import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTConnectionClosedException;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.exception.FITTESTExceptionListener;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.common.core.xml.UploadResource;
import eu.fittest.component.common.AbstractComponent;
import eu.fittest.component.junit.gui.JunitDescriptionView;
import eu.fittest.component.junit.gui.SeleniumLogHandler;
import eu.fittest.component.services.registration.spec.IComponentRegistrationService;

public class JUnitComponent extends AbstractComponent implements FITTESTExceptionListener {
	private static final String _SELENIUM_LOGFILE = "selenium.log";
	private String _sessionName;
	private List<Class<?>> _testcases;
	private SeleniumServer _selenium;
	private JUnitTestCaseTask _task;

	private IJunitDescriptionView _view;
	private Handler _loggingHandler;
	
	// by urueda
	private static String _testLogsFolder; // OS temp folder where test logs are stored
	private static String FITTEST_TEST_LOGS = "fit_test_logs";
	
	private String _fittestEntity;
	
	public JUnitComponent(int port) throws FITTESTException {
		super(JUnitComponent.class.getSimpleName(), port);
		_task = null;
		_view = new JunitDescriptionView();
		_loggingHandler = null;		
		_testLogsFolder = "file://" + System.getProperty("java.io.tmpdir") + FITTEST_TEST_LOGS; // by urueda
	}

	@Override
	public void register() throws FITTESTException {
		super.register();
		_connection.addFITTESTExceptionListener(this);
		_view.open();
	}
	
	public void initialize(List<Parameter> parameters) {
		_view.open();
		_sessionName = null;
		boolean trust = false;
		String firefoxtemplate = null;
		
		for(Parameter p: parameters) {
			Logger.getAnonymousLogger().log(Level.FINEST, p.getName()+"="+p.getValue());
			if(p.getName().equals("session.name")){
				_sessionName = p.getValue();
			}
			else if(p.getName().equals("selenium.trustAllSSLCertificates")){
				trust = Boolean.parseBoolean(p.getValue());
			}
			else if(p.getName().equals("selenium.firefoxProfileTemplate")){
				firefoxtemplate = p.getValue();
			}
			else if(p.getName().equals("report.sendTo")){
				_fittestEntity = p.getValue();
			}
		}
		Logger.getAnonymousLogger().log(Level.INFO,"session "+_sessionName+" starting");
		if(_sessionName!=null){
			String sessionFolderName = _serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir()+_sessionName+"/";
			
			RemoteControlConfiguration configuration = new RemoteControlConfiguration();
			try {
				configuration.setLogOutFile(new File(new URI(sessionFolderName+_SELENIUM_LOGFILE)));
				configuration.setUserExtensions(new File("user-extensions.js"));

				// begin by urueda (performance)
				configuration.setBrowserSideLogEnabled(false);
				configuration.setDebugMode(false);
				configuration.setDontTouchLogging(true);
				configuration.setReuseBrowserSessions(true);
				// end by urueda
				
				Logger.getAnonymousLogger().log(Level.INFO, "Trust All SSL Certificates ="+trust);
				configuration.setTrustAllSSLCertificates(trust);
				if(firefoxtemplate!=null){
					File profileFolder = new File(new URI(sessionFolderName+firefoxtemplate));
					if(!profileFolder.exists()) throw new FITTESTException("Firefox profile template "+profileFolder.getAbsolutePath()+ " does not exist");
					else{
						Logger.getAnonymousLogger().log(Level.INFO, "Firefox profile template is "+profileFolder.getAbsolutePath());
					}
					configuration.setFirefoxProfileTemplate(profileFolder);
				}
				// Set a single window mode for easy viewing
				configuration.setInteractive(true);
				configuration.setSingleWindow(true);
				
				_selenium = new SeleniumServer(configuration);
				_selenium.start();
				_loggingHandler = new SeleniumLogHandler(_view);
				Logger.getLogger("org.openqa.selenium.server").addHandler(_loggingHandler);
			} catch (Exception e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getClass().getSimpleName()+": "+e.getMessage());
			}	
			
			
			_testcases = new ArrayList<Class<?>>();
			try {						
				
				URI uri = new URI(sessionFolderName);
				URLClassLoader loader = new URLClassLoader(new URL[]{uri.toURL()});
				
				updatePropertiesFile(loader);
				
				File sessionFolder = new File(uri);
				_testcases = loadTestCases(sessionFolder, loader, "");
			} catch (URISyntaxException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			} catch (MalformedURLException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			} catch (IOException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}
		// begin by urueda
		File tmpF = new File(_testLogsFolder);
		makeDir(tmpF);
		Logger.getAnonymousLogger().log(Level.INFO, "Test logs folder = " + _testLogsFolder);
		// end by urueda
	}
	
	private void makeDir(File folder) {
		if (!folder.exists()) {
			folder.mkdirs();
			// set folder rights
	        folder.setReadable(true, false);
	        folder.setWritable(true, false);
		}
	}
	
	private void updatePropertiesFile(URLClassLoader loader) throws IOException, URISyntaxException {
		Properties  p = new Properties();
		InputStream is =loader.getResourceAsStream("test.properties"); 
		p.load(is);
		is.close();
		p.setProperty("selenium.serverPort",Integer.toString(_selenium.getConfiguration().getPort()));
		p.setProperty("selenium.serverHost","localhost");
		p.setProperty("selenium.browserStartCommand", _view.getSelectedWebBrowser().toString());
		FileOutputStream fos =new FileOutputStream(new File(loader.getResource("test.properties").toURI())); 
		p.store(fos, "updated by JUnitComponent");
		fos.close();
	}

	static private List<Class<?>> loadTestCases(File current, URLClassLoader loader, String packageName){
		List<Class<?>> testCases = new ArrayList<Class<?>>();
		for(File testcase: current.listFiles()){
			if(testcase.isFile()){
				int index =  testcase.getName().lastIndexOf(".class");
				if(index!=-1){
					Logger.getAnonymousLogger().log(Level.INFO,"loading test case "+testcase);
					try {
						Class<?> aclass = loader.loadClass(packageName+testcase.getName().substring(0,index));
						testCases.add(aclass);
					} catch (ClassNotFoundException e) {
						Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
					} 
				}
			}
			else if(testcase.isDirectory()){
				testCases.addAll(loadTestCases(testcase, loader, packageName+testcase.getName()+"."));
			}
		}
		return testCases;
	}

	public void start() {
		if(_sessionName!=null){
			_task = new JUnitTestCaseTask(_serviceRegistry, _testcases.toArray(new Class<?>[0]), _fittestEntity);
			FITTESTSingleton.getThreadPool().execute(_task);
		}
	}	

	public void stop() {
		if(_sessionName!=null){
			Logger.getAnonymousLogger().log(Level.INFO, "Stopping test case task");
			_task.interrupt();
			_task = null;
		}
	}

	public void terminate() {
		Logger.getAnonymousLogger().log(Level.INFO, "Terminate test case task");
		_selenium.stop();
		_selenium = null;
		Logger.getLogger("org.openqa.selenium.server").removeHandler(_loggingHandler);
		_loggingHandler = null;
		UploadResource upload = FITTESTSingleton.getObjectFactory().createUploadResource();
		upload.setFrom(_serviceRegistry.findService(IIdentityService.class).getMyIdentity());
		upload.setTo(_serviceRegistry.findService(IComponentRegistrationService.class).getAgentId());
		upload.setTowards(_serviceRegistry.findService(IComponentRegistrationService.class).getIteId());
		
		String compSessionDir = _serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir()+_sessionName;
		uploadOracleLogs(upload,compSessionDir);

		upload.setResource(compSessionDir+"/"+_SELENIUM_LOGFILE);
		try {
			_serviceRegistry.findService(IConnectionService.class).sendMessage(upload);
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
		
	}
	
	// by urueda
	private void uploadOracleLogs(UploadResource upload, String compSessionDir) {
		// send oracle logs to ITE, if they exist
		//Logger.getAnonymousLogger().log(Level.INFO, "Upload logs from oracle's logs folder = " + _testLogsFolder);
		String compLogsFolder = compSessionDir;
		
		String upPath = _testLogsFolder; //_serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir() + "/" + FITTEST_TEST_LOGS;
		//makeDir(new File(upPath));
		upPath = upPath + "/" + _sessionName;
		makeDir(new File(upPath));
		
		//File clf = new File(compLogsFolder);
		// set folder rights
	    //clf.setReadable(true, false);
	    //clf.setWritable(true, false);
		
		String agentLog;
		File tlf = new File(_testLogsFolder);		
		for (File file : tlf.listFiles()) {
			//Logger.getAnonymousLogger().log(Level.INFO, "Checking file: " + file.getName());				
			if (file.getName().endsWith(".log")) {
				//Logger.getAnonymousLogger().log(Level.INFO, "Going to upload: " + file.getAbsolutePath());
				//agentLog = compLogsFolder + "/" + file.getName();
				//agentLog = _testLogsFolder + "/" + "this_is_working.log"; //upPath + "/" + file.getName();
				agentLog = upPath + "/" + file.getName();
				//Logger.getAnonymousLogger().log(Level.INFO, "Agent log: " + agentLog);				
				try {
					moveLog(file.getPath(),agentLog);
					upload.setResource(agentLog);
					try {
						_serviceRegistry.findService(IConnectionService.class).sendMessage(upload);
					} catch (FITTESTException e) {
						Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
					}
				} catch (IOException ioe) {
					Logger.getAnonymousLogger().log(Level.SEVERE, ioe.getMessage());
				}
			}
		}
	}	
	
	// copy source to dest & delete source
    private static void moveLog(String source, String dest) throws IOException {
    	Logger.getAnonymousLogger().log(Level.INFO, "Move: " + source + "; to: " + dest);				
        FileReader in = null;
        FileWriter out = null;
        try {
			in = new FileReader(source);
        	out = new FileWriter(dest);
			int k;
			while( ( k = in.read() ) != -1 ) {
		        out.write(k);   		
	        }
        } finally {
            out.close();
	        in.close();
			(new File(source)).delete();            
        }
    }	
	
	public void uncaughtException(Throwable t) {
		if(t instanceof FITTESTConnectionClosedException){
			Logger.getAnonymousLogger().log(Level.INFO, "Connection to agent closed, stopping Selenium");
			if(_selenium!=null) _selenium.stop();
			_view.close();
			FITTESTSingleton.shutdown();
		}
	}
	
	public void registered(){
		_fittestEntity = _serviceRegistry.findService(IComponentRegistrationService.class).getIteId();
	}

}
