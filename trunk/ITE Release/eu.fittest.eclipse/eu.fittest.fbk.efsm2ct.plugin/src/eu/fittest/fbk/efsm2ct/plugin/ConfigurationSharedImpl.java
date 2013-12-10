package eu.fittest.fbk.efsm2ct.plugin;

import eu.fittest.project.config.TestProject;

public class ConfigurationSharedImpl implements Configuration {

	private static ConfigurationSharedImpl instance;
	
	private TestProject config;
	
	public static ConfigurationSharedImpl getInstance() {

		if (instance == null) {

			instance = new ConfigurationSharedImpl();

		}

		return instance;

	}
	

	public ConfigurationSharedImpl() {
		
		config = eu.fittest.eclipse.gui.Activator.getDefault().getActiveProjectConfig();
	}


	public String getDriverPackagePrefixRelativePath() {
		return getDriverPackagePrefix().replace(".", "/");
	}

	public String getDriverPackagePrefix() {
		return config.getTestGeneration().getSourcePackagePrefix(); // 
	}
	
	@Override
	public String getTxlHomeDirectory() {
		// return "programmi/txl10.6.linux64";
		String path = Activator.getDefault().getPreferenceStore().getString(eu.fittest.fbk.efsm2ct.plugin.preferences.PreferenceConstants.P_PATH);
		
		return path;
	}

	@Override
	public int getEvosuitePopulationSize() {
		return config.getTestGeneration().getGaParam().getPopulationSize();
	}

	@Override
	public int getEvosuiteChromosomeLength() {
		throw new UnsupportedOperationException("not implemeted yet");
		// return config.getTestGeneration().getGaParam().getChromosomeLength(); // TODO ask Cu for a field
	}

	@Override
	public int getEvosuiteStoppingPort() {
		throw new UnsupportedOperationException("not implemeted yet");
		// return config.getTestGeneration().getGaParam().getStoppingPort(); // TODO ask Cu for a field
	}


	@Override
	public String getDefaultModelFilename() {
		return config.getModelInference().getModelFile();
	}


	@Override
	public String getEventsFilterFilePath() {
		throw new UnsupportedOperationException("not implemeted yet");
	}


	@Override
	public String getConfigurationFileName() {
		throw new UnsupportedOperationException("not implemeted yet");
	}

}
