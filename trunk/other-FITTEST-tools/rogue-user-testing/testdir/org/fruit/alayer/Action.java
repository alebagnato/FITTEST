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

import java.io.Serializable;

/**
 * Actions take a system and a state as parameters and operate on the system (e.g. a left click). They
 * usually use the state to find certain values that might be necessary for their 
 * execution. In addition they might use a system's devices (Mouse, Keyboard, ...)
 * in order to execute a specific task.
 * For example: An action could always click on a menu item with the title
 * "Create New Document". Therefore it has to search the system's state
 * in order to find this menu item. It then needs to find out the item's position
 * and obtain the mouse device from the system in order to actually issue the click.
 * If it fails to do any of these tasks, it raises an exception.
 * Like states and systems, actions can have properties attached to them.
 * 
 * An action should be serializable, so that it can be stored and replayed.
 * 
 * @see SUT
 * @see State
 * @author Sebastian Bauersfeld
 */
public interface Action extends Taggable, Serializable {
	
	/**
	 * Takes a system and its state as input and executes a certain action (e.g. a click).
	 * The duration parameter indicates how fast the action should be executed. For example:
	 * If the action moves the cursor on a widget before clicking on it, a high value for
	 * this parameter will cause the mouse movement to be slowed down. However, the value
	 * is only a recommendation.
	 * @param system the SUT
	 * @param state the SUT's current state
	 * @param duration the duration of the action in seconds
	 * @throws ActionFailedException
	 */
	void run(SUT system, State state, double duration) throws ActionFailedException;
}
