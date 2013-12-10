package eu.fittest.fbk.efsm2ct.log2efsm;

/**
 * common configuration values
 * 
 * @author tiella
 * 
 */

public class Configuration {

	private static Configuration instance = new Configuration();

	private String fbkLogFilenamePrefix = "log_";

	private Configuration() {
	}

	public static Configuration getInstance() {
		return instance;
	}

	public String getFbkLogFilenamePrefix() {
		return fbkLogFilenamePrefix;
	}

	public void setFbkLogFilenamePrefix(String fbkLogFilenamePrefix) {
		this.fbkLogFilenamePrefix = fbkLogFilenamePrefix;
	}

}
