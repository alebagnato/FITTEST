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
import org.fruit.alayer.Action;
import org.fruit.alayer.State;
import org.fruit.alayer.SUT;
import org.fruit.alayer.TaggableBase;

/**
 * An action that simply waits a given amount of seconds.
 */
public final class Wait extends TaggableBase implements Action {

	private static final long serialVersionUID = 8248189921206790701L;
	private final double waitTime;
	private final boolean oveheadDuration;
	
	public Wait(double waitTime){ this(waitTime, false); }
	
	public Wait(double waitTime, boolean overheadDuration){
		Assert.isTrue(waitTime >= 0);
		this.oveheadDuration = overheadDuration;
		this.waitTime = waitTime;
	}
	
	public void run(SUT system, State state, double duration) {
		Assert.isTrue(duration >= 0);
		Util.pause(waitTime);
		if(!oveheadDuration)
			Util.pause(Math.max(0, waitTime - duration));  // sleep the rest of the time
	}
	
	public String toString(){
		return "Wait for " + (oveheadDuration ? "exactly " : "") + waitTime + " seconds";
	}	
}
