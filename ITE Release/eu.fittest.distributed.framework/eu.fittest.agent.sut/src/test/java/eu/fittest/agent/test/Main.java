package eu.fittest.agent.test;


import eu.fittest.agent.sut.core.SUTAgent;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;

public class Main {
	private SUTAgent _agent;
	
	Main() throws FITTESTException{
		_agent = new SUTAgent();
		_agent.register(HUTEnvironment.TEST, HUTType.CLIENT, System.getProperty("os.name")+", "+System.getProperty("os.version")+", "+System.getProperty("os.arch"));
	}
	
	
	/**
	 * @param args
	 * @throws FITTESTException 
	 */
	public static void main(String[] args) throws FITTESTException {	
		System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_START, FITTESTConstants.DEFAULT_FITTEST_ITE_SERVER_PORT_START.toString());
		System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_RANGE, FITTESTConstants.DEFAULT_FITTEST_ITE_SERVER_PORT_RANGE.toString());
		System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_INCREMENT, FITTESTConstants.DEFAULT_FITTEST_SERVER_PORT_INCREMENT.toString());
		System.setProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR, FITTESTConstants.DEFAULT_FITTEST_SERVICE_FILETRANSFER_BASEDIR);
		new Main();
	}

}
