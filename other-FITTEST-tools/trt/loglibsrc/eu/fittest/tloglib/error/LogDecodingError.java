package eu.fittest.tloglib.error;

/**
 * Representing errors thrown by log decoders.
 * 
 * @author Wishnu Prasetya
 * 
 */
public class LogDecodingError extends Error {
	
	private static final long serialVersionUID = 1L;

	public LogDecodingError(String s){ super(s) ; }

}
