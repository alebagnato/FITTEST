package eu.fittest.flashdriver;

import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;

import org.junit.After;
import org.junit.Before;

import com.thoughtworks.selenium.DefaultSelenium;
import com.thoughtworks.selenium.Selenium;

public class TestBase 
{
	//private Properties properties;
	private static Selenium selenium;
	private String host;
	private Integer port;
	private String browserStartCommand;
	private String browserUrl;
	private String applicationName;
	private FlashApplication flashSelenium;
	private String applicationLibs;
	private String speed = "1000";
	private long initWaitTime = 3000;
	
    protected TestBase(File settings)
        throws Exception
    {
    	Properties properties = new Properties();
    	properties.load(new FileInputStream(settings));
        host = properties.getProperty("selenium.serverHost");
        port = Integer.valueOf(properties.getProperty("selenium.serverPort"));
        browserStartCommand = properties.getProperty("selenium.browserStartCommand");
        browserUrl = properties.getProperty("selenium.browserUrl");
        applicationName = properties.getProperty("application.name");
        applicationLibs = properties.getProperty("application.libs");
        selenium = new DefaultSelenium(host,port,browserStartCommand,browserUrl);
        
        if (properties.getProperty("selenium.speed") != null)
        	speed = properties.getProperty("selenium.speed");
        
        if (properties.getProperty("selenium.initwait") != null){
        	try {
        		initWaitTime = Long.valueOf(properties.getProperty("selenium.initwait"));
        	} catch (Exception e){
        		initWaitTime = 100;
        	}
        }
        
        getContext().start();
    }

    
	@Before
	public void setUp() throws Exception { 
		flashSelenium = new FlashApplication(getContext());
		getContext().open(browserUrl);
		
		Thread.sleep(initWaitTime);
		
		// Added by Cu for demo purpose
		getContext().setSpeed(speed);
		

	}

    /**
     * Tidy up resources after every test has been executed.
     * 
     * @throws Exception if the tidy up fails
     */
    @After
    public void tearDown()
        throws Exception
    {
    	Selenium context = getContext();
    	if(context != null)
    		getContext().stop();
    }
    
    /**
     * Retrieve the context for accessing Selenium.
     * 
     * @return the Selenium context
     */
    protected static Selenium getContext()
    {
        return selenium;
    }

    
    private String retryCall(int retries, int initialDelay,String function, String...args) throws Exception
    {
	
    	boolean succes = false;
    	int delay = initialDelay;
    	String result = null;
		while(!succes && retries > 0)
		{
			retries--;
			succes = true;
			try
			{
				Thread.sleep(delay);
				delay=delay*2; // double waiting time at each try
				//Try loading the application
				result = flashSelenium.call(function, args);
			}
			catch(Exception e)
			{
				succes = false;
			}
		}
		if(succes)
		{
			return result;
		}
		else
		{
			throw new Exception("Could not call function: " + function);
		}
    }
    
	protected FlashApplication getFlashApp() throws Exception {
		/*
		try {
			String[] libs = applicationLibs.trim().split("\\s*,\\s*");
			String[] args = new String[libs.length+1];
			args[0] = applicationName;
			System.arraycopy(libs, 0, args, 1, libs.length);
			
			retryCall(10, 64, "loadApplication",args);
		} catch (Exception e) {
			throw new Exception("Could not load application: + applicationName");
		}
		try {
			retryCall(10, 64, "TestObject", "");
		} catch (Exception e) {
			throw new Exception(
					"Could not connect to ExternalInterface of Flash Application.");
		}*/
		return flashSelenium;
	}
}
