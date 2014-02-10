package eu.fittest.agent.test;

import java.util.logging.Level;
import java.util.logging.Logger;

import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.util.NetworkUtils;

public class SUTAgentTest {

	/**
	 * @param args
	 * @throws FITTESTException 
	 */
	public static void main(String[] args) throws FITTESTException {
		System.setProperty(FITTESTConstants.FITTEST_ITE_ADDRESS, NetworkUtils.getLocalIP());//in production, comes from the JNLP file
		System.setProperty(FITTESTConstants.FITTEST_ITE_PORT, FITTESTConstants.DEFAULT_FITTEST_ITE_SERVER_PORT_START.toString());//in production, comes from the JNLP file
		
		Logger.getLogger("").setLevel(Level.INFO);
		
		Main.main(null);
	}

}
