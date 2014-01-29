package eu.fittest.fbk.efsm2ct.plugin;

public class ConfigurationFactory {

	private static Configuration instance;
	
	public static Configuration getInstance() {

		if (instance == null) {

			instance = new ConfigurationSharedImpl();

		}

		return instance;

	}


}
