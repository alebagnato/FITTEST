package eu.fittest.logging.trtransf.classAnalysis;

/**
 * Just a container method to hold a result object. This is used to hold
 * a global state manipulated from within visitors in a visitor pattern.
 * The way that we program it requires such a global state to be declred
 * as final in Java, but then it cannot be modified. So, this container
 * is used just as a trick to circumvent this.
 * 
 * @param <T>
 */
public class FinalResult<T> {
	public T value;
	
	public FinalResult() {
	}
	
	public FinalResult(T initialValue) {
		this.value = initialValue;
	}
}
