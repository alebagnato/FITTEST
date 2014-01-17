package eu.fittest.fbk.efsm2ct.flexdrv.logging;

import static org.junit.Assert.*;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

public class LoggerManagerTest {

	// @Test
	public void test() {
		
		LoggerManager.initialize();
		
		Logger logger = Logger.getLogger(LoggerManager.LOGGER);
		
		logger.setLevel(Level.FINEST);
		
		logger.finest("it should be finest");
		logger.finer("it should be finer");
		logger.fine("it should be fine");
		logger.info("it should be info");
		logger.warning("it should be warning");
		logger.severe("it should be severe");
		
		
	}

}
