package eu.fittest.fbk.efsm2ct.plugin;

public interface Configuration {

	static public String genSrcPath = "/gensrc";
	
	String getDriverPackagePrefixRelativePath();
	String getDriverPackagePrefix();
	String getTxlHomeDirectory();
	int getEvosuitePopulationSize();
	int getEvosuiteChromosomeLength();
	int getEvosuiteStoppingPort();
	String getDefaultModelFilename();
	String getEventsFilterFilePath();
	String getConfigurationFileName();
	boolean isVerboseLogView();
	boolean isLogView();
	boolean isLogViewTrasitions();
	int getEvosuiteGenerations();
	

}
