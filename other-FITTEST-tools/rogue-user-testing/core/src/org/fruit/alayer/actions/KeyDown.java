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
package org.fruit.alayer.actions;

import org.fruit.Assert;
import org.fruit.Util;
import org.fruit.alayer.ActionFailedException;
import org.fruit.alayer.Action;
import org.fruit.alayer.State;
import org.fruit.alayer.SUT;
import org.fruit.alayer.NoSuchTagException;
import org.fruit.alayer.TaggableBase;
import org.fruit.alayer.Tags;
import org.fruit.alayer.devices.KBKeys;

/**
 * An action which presses a given Key on the Keyboard.
 */
public final class KeyDown extends TaggableBase implements Action {
	private static final long serialVersionUID = -462251384755779329L;
	private final KBKeys key;
		
	public KeyDown(KBKeys key){
		Assert.notNull(key);
		this.key = key;
	}
	
	public String toString() { return "Press Key " + key; }

	public void run(SUT system, State state, double duration) {
		try{
			Assert.notNull(system);
			Util.pause(duration);
			system.get(Tags.StandardKeyboard).press(key);
		}catch(NoSuchTagException tue){
			throw new ActionFailedException(tue);
		}
	}
}
