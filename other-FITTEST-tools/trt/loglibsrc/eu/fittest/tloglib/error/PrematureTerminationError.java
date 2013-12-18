package eu.fittest.tloglib.error;

/**
 * No more input to process in the decoded log. This is not necessarily an error;
 * it could be that the target application crashed in the middle.
 */
public class PrematureTerminationError extends LogDecodingError {

	private static final long serialVersionUID = 1L;

	public PrematureTerminationError(String s) { super(s) ; }
}
