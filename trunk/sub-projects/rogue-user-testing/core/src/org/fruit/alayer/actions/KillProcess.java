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

import java.util.Collections;
import org.fruit.Assert;
import org.fruit.Util;
import org.fruit.alayer.Action;
import org.fruit.alayer.ActionFailedException;
import org.fruit.alayer.SUT;
import org.fruit.alayer.State;
import org.fruit.alayer.TaggableBase;
import org.fruit.alayer.Tags;
import org.fruit.alayer.devices.ProcessHandle;

/**
 * An action that kills a process by PID or by Name.
 */
public class KillProcess extends TaggableBase implements Action {
	private static final long serialVersionUID = -1777427445519403935L;
	final String name;
	final Long pid;
	final double waitTime;

	public static KillProcess byName(String name, double timeToWaitForProcessToAppear){ return new KillProcess(name, null, timeToWaitForProcessToAppear); }
	public static KillProcess byPID(long pid, double timeToWaitForProcessToAppear){ return new KillProcess(null, pid, timeToWaitForProcessToAppear); }

	private KillProcess(String name, Long pid, double waitTime){
		Assert.isTrue(!(name == null && pid == null) && waitTime >= 0);
		this.name = name;
		this.pid = pid;
		this.waitTime = waitTime;
	}

	public void run(SUT system, State state, double duration) throws ActionFailedException{
		Assert.notNull(system);
		Assert.isTrue(duration >= 0);
		
		double start = Util.time();
		Util.pause(waitTime);
		
		for(ProcessHandle ph : Util.makeIterable(system.get(Tags.ProcessHandles, Collections.<ProcessHandle>emptyList().iterator()))){
			if((pid != null && ph.pid() == pid) || (name != null && name.equals(ph.name()))){
				ph.kill();
				return;
			}
		}
		
		Util.pause(duration - (Util.time() - start));
	}
	
	public String toString(){ return "KillProcess"; }
}
