package eu.fbk.xinputmining;

public class MiningException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 3761956224485257756L;

	private String message;

	public String getMessage() {
		return message;
	}

	public MiningException(String message) {
		super();
		this.message = message;
	}
	
}
