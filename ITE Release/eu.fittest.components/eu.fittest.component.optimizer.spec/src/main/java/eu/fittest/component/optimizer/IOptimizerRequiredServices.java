package eu.fittest.component.optimizer;

import java.io.File;
import java.util.Properties;

import eu.fittest.common.core.exception.FITTESTException;

/**
 * @author cballagny
 *
 */
public interface IOptimizerRequiredServices {
	/** start a Java application instrumented with ConTest configured with the given set of King properties and executes a set of test cases
	 * When the time limit is reached or when test cases execution is completed, it returns the path to the folder containing the log files
	 * (e.g. /tmp/sessionid/com_ibm_contest/)
	 * 
	 * @param kingProperties: set of King properties whose name are the same as the one specified in the KingProperties file
	 * @param timeLimit: maximum duration of the test case execution
	 * @return folder containing the contest log files (e.g. com_ibm_contest)
	 * @throws FITTESTException 
	 */
	public File execute(Properties kingProperties) throws FITTESTException;
	
	/**report the progress of the optimizer. the value must be between 0 and 100 included
	 * @param value
	 */
	public void setProgress(int value);
}
