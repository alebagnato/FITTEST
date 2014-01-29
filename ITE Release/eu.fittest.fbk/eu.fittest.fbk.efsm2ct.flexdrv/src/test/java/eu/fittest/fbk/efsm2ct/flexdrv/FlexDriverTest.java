package eu.fittest.fbk.efsm2ct.flexdrv;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import eu.fittest.fbk.efsm2ct.flexdrv.logging.LoggerManager;

public class FlexDriverTest {

	private static String seleniumHost = "192.168.56.101"; // "localhost"; //

	// @Test
	public void test1() {

		LoggerManager.initialize();

		Logger logger = Logger.getLogger(LoggerManager.LOGGER);

		logger.setLevel(Level.FINEST);

		System.setProperty(FlexDriver.SELENIUMHOST_PROPERTY, seleniumHost);

		FlexDriver driver = new FlexDriver();

		try {

			driver.startupDaemons();

			driver.startSelenium();

			driver.startApplication();

			logger.info("sleeping ...");
			Thread.sleep(5000);
			logger.info("wake up.");

			driver.dismissApplication();
			driver.stopDaemons();
			
			driver.shutdown();

		} catch (Exception e) {
			System.err.println("can't start");
			e.printStackTrace();
			try {
				driver.shutdown();
			} catch (Exception e1) {
				System.err.println("can't shutdown in handling starting failure");
				e.printStackTrace();
			}
			fail(e.toString());
		}

	}

	// @Test
	public void test2() {

		LoggerManager.initialize();
		Logger logger = Logger.getLogger(LoggerManager.LOGGER);
		logger.setLevel(Level.INFO);

		boolean failed = false;

		System.setProperty(FlexDriver.SELENIUMHOST_PROPERTY, seleniumHost);

		FlexDriver driver = new FlexDriver();

		try {

			driver.startupDaemons();

			driver.startSelenium();

			driver.startApplication();

			logger.info("sleeping ...");
			Thread.sleep(1000);
			logger.info("wake up.");

			try {

				StateDescription sd = driver.invoke("ButtonBar0", "itemclick", Integer.toString(1));

				System.out.println(sd);

				sd = driver.invoke("priceSlider", "change", Integer.toString(10), Integer.toString(100));

				System.out.println(sd);

				sd = driver.invoke("priceSlider", "change", Integer.toString(120), Integer.toString(500));

				System.out.println(sd);

			} catch (FlexDriverException ex) {
				ex.printStackTrace();
				failed = true;
			}

			driver.shutdown();

		} catch (Exception e) {
			e.printStackTrace();
			failed = true;
		}

		assertFalse(failed);

	}

	@Test
	public void test3() {

		LoggerManager.initialize();

		Logger logger = Logger.getLogger(LoggerManager.LOGGER);

		logger.setLevel(Level.FINEST);

		System.setProperty(FlexDriver.SELENIUMHOST_PROPERTY, seleniumHost);

		FlexDriver driver = new FlexDriver();

		try {
			driver.startupDaemons();

			driver.startSelenium();

			driver.startApplication();

			for (int i = 0; i < 10; i++) {

				logger.info("sleeping ...");
				Thread.sleep(500);
				logger.info("wake up.");

				StateDescription sd = driver.invoke("ButtonBar0", "itemclick", Integer.toString(1));
				logger.info("first: " + sd);

				logger.info("sleeping ...");
				Thread.sleep(500);
				logger.info("wake up.");

				driver.dismissApplication();
				driver.stopDaemons();

				logger.info("sleeping ...");
				Thread.sleep(500);
				logger.info("wake up.");

				driver.startupDaemons();
				driver.startApplication();

			}

			logger.info("sleeping ...");
			Thread.sleep(500);
			logger.info("wake up.");

			driver.shutdown();

		} catch (Exception e) {
			e.printStackTrace();
			fail(e.toString());
		}

	}

}
