package eu.fittest.component.phplogger;

import eu.fittest.common.core.exception.FITTESTException;

public class Main {

	/**
	 * @param args
	 * @throws FITTESTException 
	 * @throws NumberFormatException 
	 */
	public static void main(String[] args) throws NumberFormatException, FITTESTException {
		if(args.length!=1){
			System.out.println("Usage: java -jar eu.fittest.component.phplogger.Main <SUT agent server port>");
		}
		else{
			new PHPLoggerComponent(Integer.parseInt(args[0])).register();
		}
	}

}
