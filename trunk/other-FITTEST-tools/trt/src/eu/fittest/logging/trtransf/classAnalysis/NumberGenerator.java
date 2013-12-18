package eu.fittest.logging.trtransf.classAnalysis;

/**
 * Generates unique variable names
 *
 */
public class NumberGenerator {
	
	int value = 0;
	
	/**
	 * Generate next unique number
	 * @return returns an integer
	 */
	public Integer next() {
		this.value++;
		return this.value;
	}
}









