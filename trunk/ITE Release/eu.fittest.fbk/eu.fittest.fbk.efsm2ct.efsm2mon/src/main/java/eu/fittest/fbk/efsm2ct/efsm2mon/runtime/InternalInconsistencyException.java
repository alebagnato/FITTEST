/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.runtime;

/**
 * 
 * @author tiella
 */
public class InternalInconsistencyException extends FsmtesterException {

	/**
	 * Creates a new instance of <code>UnderSpecifiedException</code> without
	 * detail message.
	 */
	public InternalInconsistencyException() {
	}

	/**
	 * Constructs an instance of <code>UnderSpecifiedException</code> with the
	 * specified detail message.
	 * 
	 * @param msg
	 *            the detail message.
	 */
	public InternalInconsistencyException(String msg) {
		super(msg);
	}
}
