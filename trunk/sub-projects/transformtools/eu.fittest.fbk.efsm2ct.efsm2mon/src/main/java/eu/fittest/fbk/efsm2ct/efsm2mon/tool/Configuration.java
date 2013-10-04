package eu.fittest.fbk.efsm2ct.efsm2mon.tool;

/**
 * 
 * @author tiella
 */
public class Configuration {

	private static Configuration instance;

	public static Configuration getInstance() {
		if (instance == null) {

			instance = new Configuration();

		}

		return instance;
	}

	public String getCompilerDestDir() {
		return System.getProperty("fsmtester.compiler.dest.dir");
	}

}
