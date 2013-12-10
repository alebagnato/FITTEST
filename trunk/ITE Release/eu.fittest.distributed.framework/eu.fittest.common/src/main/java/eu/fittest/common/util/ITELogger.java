package eu.fittest.common.util;

import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * This logger wrap the system eu.fittest.common.util.FittestLogger to avoid 
 * a null pointer exception risen after Java 6 update 51
 * 
 * 
 * @author cdnguyen
 *
 */
public class ITELogger {
	
	/**
	 * Log method
	 * @param level
	 * @param msg
	 */
	public static void log(Level level, String msg){
		Logger logger = null;
		try {
			logger = Logger.getLogger("FITTEST Logger");
		} catch (Exception e){
			e.printStackTrace();
		}
		
		if (logger != null){
			logger.log(level, msg);
		} else {
			System.out.println(level.getName() + ": " + msg);
		}
	}

	/**
	 * Log info only
	 * @param msg
	 */
	public static void info(String msg) {
		Logger logger = null;
		try {
			logger = Logger.getLogger("FITTEST Logger");
		} catch (Exception e){
			e.printStackTrace();
		}
		
		if (logger != null){
			logger.log(Level.INFO, msg);
		} else {
			System.out.println(Level.INFO + ": " + msg);
		}
	}
	
}
