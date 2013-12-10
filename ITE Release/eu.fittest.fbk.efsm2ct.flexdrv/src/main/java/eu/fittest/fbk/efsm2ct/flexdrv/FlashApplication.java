package eu.fittest.fbk.efsm2ct.flexdrv;

import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.thoughtworks.selenium.FlashSelenium;
import com.thoughtworks.selenium.Selenium;

import eu.fittest.fbk.efsm2ct.flexdrv.logging.LoggerManager;

public class FlashApplication extends FlashSelenium {

	
	
	private static final Logger log = Logger.getLogger(LoggerManager.LOGGER);
	private static final String AUTOMATION_LOADER = "AutomationLoader";

	private Level level = Level.FINE;

	public FlashApplication(Selenium selenium) {
		super(selenium, AUTOMATION_LOADER);
		level=Level.parse(System.getProperty(FlexDriver.WARNINGLEVEL_PROPERTY, level.toString()));
	}

	public String invoke(String objectID, String method, String... args) {
		return invoke(1, 1000, objectID, method, args);
	}

	public String invoke(int tries, int delay, String objectID, String method, String... args) {

		while (tries > 0) {

			tries--;

			log("calling TestObject: " + objectID);
			String result = call("TestObject", objectID);
			log("result: " + result);

			if ("true".equals(result)) {
				String[] arguments = new String[args.length + 2];
				arguments[0] = objectID;
				arguments[1] = method;
				System.arraycopy(args, 0, arguments, 2, args.length);

				log("invoking: " + Arrays.asList(arguments));
				String result2 = call("Invoke", arguments);
				log("result:" + result2);

				return result2;
			}

			if (tries > 0) {
				try {
					log.log(level,"wait before retrying: " + delay);
					Thread.sleep(delay);
				} catch (InterruptedException e) { // ignore interrupts
				}
			}

		}

		log.log(level,"Could not find object with id: " + objectID);

		return null;
	}

	private void log(String msg) {
		log.fine(Thread.currentThread() + " " + msg);
	}
}
