/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.runtime;

/**
 * 
 * @author tiella
 */
public class SutCommunicationException extends FsmtesterException {

	/**
	 * Constructs an instance of <code>SutCommunicationException</code> with the
	 * specified detail message.
	 * 
	 * @param msg
	 *            the detail message.
	 */
	public SutCommunicationException(String msg) {
		super(msg);
	}

	public SutCommunicationException(String message, Throwable cause) {
		super(message, cause);
	}

	public SutCommunicationException(Throwable cause) {
		super(cause);
	}

}
