/**************************************************************************************
*  Copyright (c) 2013, Universitat Politecnica de Valencia. All rights reserved.      *
*  This program and the accompanying materials are made available under the terms     *
*  of the 3-Clause BSD License which accompanies this distribution, and is available  *
*  at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these  *
*  results has received funding from the European Community`s Seventh Framework       *
*  Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.           *
**************************************************************************************/

/**
 *  @author Sebastian Bauersfeld
 */
package org.fruit.alayer;

/**
 * An SUT is a System Under Test and can be a graphical application, a process, a collection of processes or
 * even a collection of distributed services. A system is always in a specific <code>State</code>. <code>Action</code>'s
 * operate on a system and its state.
 * @see Action
 * @see State
 */
public interface SUT extends Taggable{
	
	/**
	 * Stops execution of the system. Implementations should try to first shut down
	 * the system "gently" and gradually get more aggressive if that fails. If it is
	 * not possible to shutdown the system, implementors of this method are supposed to throw a
	 * <code>SystemStopException</code>.
	 * @throws SystemStopException if the system cannot be stopped
	 */
	void stop() throws SystemStopException;
    
	/** Is the system running?
	 * @return returns whether the system is running
	 */
	boolean isRunning();
}
