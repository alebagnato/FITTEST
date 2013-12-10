package eu.fittest.component.optimizer;

import java.io.File;
import java.util.Properties;


/**
 * @author cballagny 
 *
 */
public interface IOptimizerProvidedServices {
	String TARGET_CLASSES = "targetClasses";
	String SEARCH_ALGORITHM = "optimizer.searchAlgorithm";
	String RANDOM_NUMBER_SEED = "optimizer.randomNumberSeed";

	/**
	 * set the interface required by the Optimizer to execute the test cases
	 * @param requiredServices
	 */
	public void setOptimizerRequiredServices(IOptimizerRequiredServices requiredServices);
	/**
	 * run the optimizer, when this method returns, optimization is completed (or timed out) and a KingProperty file is available for results 
	 */
	public void run();
	
	/**
	 * specify where the KingProperties file must be generated at the end of the optimizer run
	 * @param folder
	 */
	public void setOutputFolder(File folder);
	
	/**
	 * set the couple of King Properties values required to initialize the optimer like targetClasses value
	 * @param properties
	 */
	public void setSeedProperties(Properties properties);
	
	public void setRandomNumberSeed(Long seed);
	
	public void setSearchAlgorithm(SearchAlgorithm value);
	
	/**
	 * interrupt the optimizer 
	 */
	public void interrupt();
}
