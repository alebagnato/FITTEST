package eu.fittest.component.junit;

import eu.fittest.common.core.exception.FITTESTException;

public class Main {

	/**
	 * @param args
	 * @throws FITTESTException 
	 * @throws NumberFormatException 
	 */
	public static void main(String[] args) throws NumberFormatException, FITTESTException {
		if(args.length!=1){
			System.out.println("Usage: java -jar eu.fittest.component.junit.Main <SUT agent server port>");
		}
		else{
			new JUnitComponent(Integer.parseInt(args[0])).register();
		}
	}

}
