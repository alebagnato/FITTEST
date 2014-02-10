package eu.fittest.fbk.efsm2ct.plugin;

import eu.fittest.project.config.TestProject;

public class ConfigurationSharedImpl implements Configuration {

	private static ConfigurationSharedImpl instance;
	
	// private TestProject config;
	
	public static ConfigurationSharedImpl getInstance() {

		if (instance == null) {

			instance = new ConfigurationSharedImpl();

		}

		return instance;

	}
	

	public ConfigurationSharedImpl() {
		
		
	}


	protected TestProject config() {
		return eu.fittest.eclipse.gui.Activator.getDefault().getActiveProjectConfig();
	}
	
	public String getDriverPackagePrefixRelativePath() {
		return getDriverPackagePrefix().replace(".", "/");
	}

	public String getDriverPackagePrefix() {
		return config().getTestGeneration().getSourcePackagePrefix(); // 
	}
	
	@Override
	public String getTxlHomeDirectory() {
		// return "programmi/txl10.6.linux64";
		String path = Activator.getDefault().getPreferenceStore().getString(eu.fittest.fbk.efsm2ct.plugin.preferences.PreferenceConstants.P_PATH);
		
		return path;
	}

	@Override
	public int getEvosuitePopulationSize() {
		return config().getTestGeneration().getGaParam().getPopulationSize();
	}

	@Override
	public int getEvosuiteChromosomeLength() {
		
		 return config().getTestGeneration().getGaParam().getChromosomeLength(); 
	}

	@Override
	public int getEvosuiteStoppingPort() {
		
		 return config().getTestGeneration().getGaParam().getStopPort();
	}


	@Override
	public String getDefaultModelFilename() {
		return config().getModelInference().getModelFile();
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
		
		return false;
		
	}
	
	@Override
	public boolean isLogView() {
		
		return true;
		
	}
	
	@Override
	public boolean isLogViewTrasitions() {
		return false; // set to true for logging transitions
	}


	@Override
	public int getEvosuiteGenerations() {
		return config().getTestGeneration().getGaParam().getMaxNumberOfGenerations();
	}
	
}
