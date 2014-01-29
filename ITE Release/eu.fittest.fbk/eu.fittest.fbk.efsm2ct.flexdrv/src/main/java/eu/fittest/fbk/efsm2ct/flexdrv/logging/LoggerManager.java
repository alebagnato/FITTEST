/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.flexdrv.logging;

import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * 
 * @author tiella
 */
public class LoggerManager {

	public static boolean initialized = false;

	// static {
	// new LoggerManager();
	// }

	public static final String LOGGER = "flexdriver";
	private static final Logger log = Logger.getLogger(LOGGER);

	public static void initialize() {

		if (!initialized) {

			initialized = true;

			// log.setLevel(Level.FINEST);

			log.fine("log manager started.");
			log.fine("setting up handlers");
			LoggerFormatter fmt = new LoggerFormatter();

			Handler h = new ConsoleHandler();
			h.setFormatter(fmt);
			// h.setLevel(Level.ALL);
			log.addHandler(h);
			log.setUseParentHandlers(false);
			// log.setLevel(Level.ALL);

			log.fine("handlers configured.");

		}

	}

}
