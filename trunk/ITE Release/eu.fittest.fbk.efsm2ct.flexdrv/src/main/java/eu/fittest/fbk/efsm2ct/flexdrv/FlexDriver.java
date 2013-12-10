/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.flexdrv;

// import com.thoughtworks.selenium.DefaultSelenium;
import java.io.IOException;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.thoughtworks.selenium.DefaultSelenium;
import com.thoughtworks.selenium.FlashSelenium;

import eu.fittest.fbk.efsm2ct.flexdrv.logging.LoggerManager;

/**
 * 
 * @author tiella
 */
public class FlexDriver {

	public static final String WARNINGLEVEL_PROPERTY = "eu.fittest.fbk.efsm2ct.flexdrv.warninglevel";
	public static final String SELENIUMHOST_PROPERTY = "eu.fittest.fbk.efsm2ct.flexdrv.seleniumhost";
	public static final String SELENIUMOPENDELAY_PROPERTY = "eu.fittest.fbk.efsm2ct.flexdrv.seleniumopendelay";
	public static final String SUTHOMEURL_PROPERTY = "eu.fittest.fbk.efsm2ct.flexdrv.suthomeurl";
	
	
	private static final int DEFAULT_DATA_PORT = 37651;
	private static final int DEFAULT_COMMAND_PORT = 37600;
	
	private static long DEFAULT_SELENIUM_OPEN_DELAY = 2000;
	
	static final Logger log = Logger.getLogger(LoggerManager.LOGGER);
	
	private static final String SELENIUM_MAIN_WINDOW = "null"; // yes, it is! "null" is the name of the main window
	private static final String SUT_WINDOW = "w1";
	private static final String[] SUT_COMPONENTS = { "flexstore.swf", "FlexDelegates.swf", "logger.swf" };

	private DefaultSelenium selenium;
	private FlashSelenium flashSelenium;
	private LogServerCommand logServerCmd;
	private LogServerLogger logServerData;
	private LogParser parser;
	
	
	
	private int dataPort = DEFAULT_DATA_PORT;
	private int commandPort = DEFAULT_COMMAND_PORT;
	
	private String seleniumHost = "localhost";
	private long seleniumOpenDelay = DEFAULT_SELENIUM_OPEN_DELAY; // millis
	
	private String homeUrl = "flexstore.html";
	
	private SocketList openSocketes = new SocketList();
	private boolean applicationBound;

	public FlexDriver() {
		setFromProperties();
		parser = new LogParser();
	}

	private void setFromProperties() {
		
		seleniumHost = System.getProperty(SELENIUMHOST_PROPERTY,
				seleniumHost);
		
		homeUrl = System.getProperty(SUTHOMEURL_PROPERTY,
				homeUrl);
		
		seleniumOpenDelay = Long.parseLong(System.getProperty(SELENIUMOPENDELAY_PROPERTY,
				Long.toString(seleniumOpenDelay)));
		
	}
	
	public void addLogConsumer(LogConsumer c) {
		
		logServerData.addLogConsumer(c);
		
	}

//	public void start() throws IOException, FlexDriverException {
//		try {
//			
//			startupDaemons();
//			
//			startSelenium();
//
//			startApplication();
//
//		} catch (IOException ex) {
//
//			throw ex;
//		}
//
//	}

	public void startApplication() {
		
		// loadHomePage();

		loadFlashApplication();

		sendInitializeLogging();

		sendStartLogging();
		
		applicationBound = true;
	}

//	private void loadHomePage() {
//
//		selenium.openWindow("flexstore.html",SUT_WINDOW);  // opens the flexstore page including the flash plug in, not the flexstore app
//		selenium.selectWindow(SUT_WINDOW);
//		
//		flashSelenium = new FlashSelenium(selenium, "AutomationLoader");
//		log("seleniuymObject=" + flashSelenium);
//		
//		try {
//			Thread.sleep(seleniumOpenDelay);
//		} catch (InterruptedException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		
//	}

	

	public void startupDaemons() throws IOException, FlexDriverException {
		
		openSocketes.closeAll();
		
		logServerData = new LogServerLogger(openSocketes, dataPort, dataPort+1);
		logServerData.start();
		
		
		int dataPort = logServerData.getSsocket().getLocalPort();
		
		logServerCmd = new LogServerCommand(openSocketes, seleniumHost, dataPort, commandPort, commandPort+1);
		logServerCmd.start();
	}

	private void debug(String msg) {
		log.finer(msg);
	}

	private void log(String msg) {
		log.fine(msg);
	}

	public void dismissApplication() {

//		log("sending stop logging message");
//		sendStopLogging();
//		log("sending terminate logging message");
//		sendTerminateLogging();
//		
//		// wait for disconnection ...
//		try {
//			Thread.sleep(15000);
//		} catch (InterruptedException e) {
//			// TODO Auto-generated catch block
//			log.log(Level.SEVERE, "can't sleep", e);
//		}
		
		
		selenium.close();
		
		logServerCmd.stopWorkers();
		logServerData.stopWorkers();
		
		selenium.selectWindow(SELENIUM_MAIN_WINDOW);
		
		applicationBound = false;
	}
	
	public void shutdown() {
		
		if (applicationBound) {
			dismissApplication();
		}
		
		if (selenium != null) {
			log("stopping selenium");
			selenium.stop();
		}

		log("stopping daemons");
		stopDaemons();
	}

	public void stopDaemons() {

		if (logServerCmd != null) {
			log("stopping cmd server");
			logServerCmd.stop();
			logServerCmd = null;
		} else {
			log("cmd server not running");
		}

		if (logServerData != null) {
			log("stopping data server");
			logServerData.stop();
			logServerData = null;
		} else {
			log("cmd server not running");
		}
	}

	public StateDescription invoke(String obj, String mth, String... args) throws FlexDriverException {
		return invoke(5, 1000, obj, mth, args);
	}
	
	public StateDescription invoke(int tries, long timeout, String obj, String mth, String... args) throws FlexDriverException {

		StateDescription state = null;

		FlashApplication app = new FlashApplication(selenium);

		// TODO serialize send/receive action

		log("ready to invoke:" + obj + "," + mth + "," + Arrays.asList(args));

		logServerData.readyToSend();

		while (tries > 0) {

			log("tries left:" + tries);
			tries--;

			app.invoke(1, 0, obj, mth, args); // null is always returned

			// log("result:" + res + " of invoked:" + obj + "," + mth + "," +
			// Arrays.asList(args));

			if (logServerData.waitDataAvailableAndLock()) {

				SynchronizedBuffer db = logServerData.getData();

				String evst = db.getBufferContent();

				log("evst:" + evst);

				state = parser.parse(evst);

				log("received:" + state.toString());

				logServerData.unlockData();

				try {
					Thread.sleep(timeout);
				} catch (InterruptedException ex) {
					log("interrupted:" + ex.toString());
				}

				return state;
			}

		}

		log("no tries left");

		throw new FlexDriverException("no answer for the application requesting " + obj + "," + mth + "," + Arrays.asList(args));

	}

	private void sendStartLogging() {
		logServerCmd.sendStartLogging();
	}

	private void sendInitializeLogging() {
		logServerCmd.sendInitializeLogging();
	}

	private void sendTerminateLogging() {

		if (logServerCmd != null) {

			logServerCmd.sendTerminatedLogging();

		}
	}

	private void sendStopLogging() {

		if (logServerCmd != null) {

			logServerCmd.sendStopLogging();

		}
	}

	public void startSelenium() {
		
		Integer seleniumPort = 4444;
		String browserConfiguration = "*googlechrome";
		String browserUrl = "http://localhost:8000/";

		selenium = new DefaultSelenium(seleniumHost, seleniumPort, browserConfiguration, browserUrl);
		selenium.start();  // synchronous call, it returns only when selenium is ready
		
	}
	
	private void loadFlashApplication() {

		selenium.openWindow(homeUrl,SUT_WINDOW);  // opens the flexstore page including the flash plug in, not the flexstore app
		
		try {
			Thread.sleep(seleniumOpenDelay);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		selenium.selectWindow(SUT_WINDOW);
		
		flashSelenium = new FlashSelenium(selenium, "AutomationLoader");
		log("seleniuymObject=" + flashSelenium);
		
		
		
		log("calling loadApplication: "+Arrays.asList(SUT_COMPONENTS));

		flashSelenium.call("loadApplication", SUT_COMPONENTS); // asynchronous

		log("loadApplication called");

		while (true) {

			if (logServerCmd.isConnectionSetupCompleted()) {
				log("connection setup completed.");

				break;
			}

			try {
				log.finest("waiting for connection completion ...");
				Thread.sleep(250);
			} catch (InterruptedException ex) {
				log("interrupted:" + ex.toString());
			}

		}

	}

	public void startLoggingIdleMessagesToFile() {

		logServerData.setLoggingIdleMessagesToFile(true);

	}
	
	public void stopLoggingIdleMessagesToFile() {

		logServerData.setLoggingIdleMessagesToFile(false);

	}
}
