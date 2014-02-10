/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.runtime;

/**
 * 
 * @author tiella
 */
public abstract class FsmtesterException extends Exception {

	/**
	 * Creates a new instance of <code>UnderSpecifiedException</code> without
	 * detail message.
	 */
	public FsmtesterException() {
	}

	/**
	 * Constructs an instance of <code>UnderSpecifiedException</code> with the
	 * specified detail message.
	 * 
	 * @param msg
	 *            the detail message.
	 */
	public FsmtesterException(String msg) {
		super(msg);
	}

	public FsmtesterException(String message, Throwable cause) {
		super(message, cause);
	}

	public FsmtesterException(Throwable cause) {
		super(cause);
	}

}
