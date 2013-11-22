/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

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
