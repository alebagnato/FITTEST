package eu.fittest.fbk.efsm2ct.plugin;

import eu.fittest.fbk.efsm2ct.plugin.preferences.PreferenceConstants;



public class ConfigurationLocalImpl implements Configuration {

	private String driverPackagePrefix = "eu.fittest.efsm2ct.sut";
	
	private static ConfigurationLocalImpl instance;

	public static Configuration getInstance() {

		if (instance == null) {

			instance = new ConfigurationLocalImpl();

		}

		return instance;

	}

	@Override
	public String getDriverPackagePrefixRelativePath() {
		return driverPackagePrefix.replace(".", "/");
	}

	@Override
	public String getDriverPackagePrefix() {
		return  driverPackagePrefix;
	}
	
	@Override
	public String getTxlHomeDirectory() {
		// return "programmi/txl10.6.linux64";
		String path = Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.P_PATH);
		
		return path;
	}

	@Override
	public int getEvosuitePopulationSize() {
		return 10;
	}

	@Override
	public int getEvosuiteChromosomeLength() {
		return 20;
	}

	@Override
	public int getEvosuiteStoppingPort() {
		return 7000;
	}

	@Override
	public String getDefaultModelFilename() {
		return "Models/sutname.efsm";
	}

	@Override
	public String getEventsFilterFilePath() {
		return "events_filter.txt";
	}

	@Override
	public String getConfigurationFileName() {
		
		return "efsm.properties";
	}

	@Override
	public boolean isVerboseLogView() {
		
		return false; // set to true to log evosuite output literally 
		
	}
	
	@Override
	public boolean isLogView() {
		
		return true;  // set to true for logging
		
	}

	@Override
	public boolean isLogViewTrasitions() {
		return false; // set to true for logging transitions
	}
	
	@Override
	public int getEvosuiteGenerations() {
		return 1000000; // evosuite default
	}
	
}
