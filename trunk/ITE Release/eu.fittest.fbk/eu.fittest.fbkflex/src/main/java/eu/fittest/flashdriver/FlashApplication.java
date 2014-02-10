package eu.fittest.flashdriver;

import junit.framework.Assert;

import com.thoughtworks.selenium.FlashSelenium;
import com.thoughtworks.selenium.Selenium;
import java.io.* ;


public class FlashApplication extends FlashSelenium {
	private static final String AUTOMATION_LOADER = "AutomationLoader";
	
	private static String _testLogsFolder;

	public FlashApplication(Selenium selenium) 
	{
		super(selenium, AUTOMATION_LOADER);
		_testLogsFolder = System.getProperty("java.io.tmpdir") + "fit_test_logs";
	}
	
	public String invoke(String objectID, String method, String... args)
	{
		return invoke(5,1000, objectID, method, args);
	}
	public String invoke(int tries, int delay, String objectID, String method, String... args)
	{
		try 
		{
			Thread.sleep(delay);
		} catch (InterruptedException e) 
		{ // ignore interrupts
		}
        String testObjectResult = null ;
        String invokeResult = null ;
		while(tries > 0)
		{
			tries--;
			testObjectResult = call("TestObject", objectID);
			if("true".equals(testObjectResult) )
			{
				String[] arguments = new String[args.length+2];
				arguments[0] = objectID; 
				arguments[1] = method;
				System.arraycopy(args, 0, arguments, 2, args.length);
                invokeResult = call("InvokeAndCheck",  arguments) ;
                System.err.println(">> InvokeAndCheck(" +  objectID + "," + method + ",...) returns : " + invokeResult) ;
                if ("OK".equals(invokeResult)) {
                    String exc = call("GetUException") ;
                    if (exc.equals("")) {
                      // Invoke OK, and no exception thrown by handlers
                      return invokeResult ;
                    }
                    else {
                      Assert.fail("Invoke(" + objectID + "," + method + ",..) throws an exception: " + exc) ;
                    }
                   
                }
			}
		}
		// Assert.fail("Could not find object with id: "+objectID);
        if ("false".equals(testObjectResult)) 
            throw new IllegalArgumentException("Object " 
                + objectID
                + " does not exists.") ;
                
        throw new IllegalArgumentException("Could not execute Invoke(" 
                + objectID
                + "," + method
                + ",..) : " + invokeResult) ;
	}
	
	public void savelog(String fname) {
       String log = call("LoggerGetLog", new String[0]) ;
	   // begin by urueda
	   if (log != null) {
	   	   String testLogPath = _testLogsFolder + "/" + fname;
		   System.out.println("Will save log to: " + testLogPath);         
		   try {
			  FileWriter out = new FileWriter(testLogPath);
			  out.write(log) ;
			  out.close() ;
			  File f = new File(testLogPath);
			  // set folder rights
	          f.setReadable(true, false);
	          f.setWritable(true, false);
		  } catch(Exception e) { System.err.println("Failed to pull the log, or to write it to a file: " + e) ; } 		
		}
		else {
			System.out.println("Test case produced null log, error?");
		}
		// end by urueda         
    }	
}
