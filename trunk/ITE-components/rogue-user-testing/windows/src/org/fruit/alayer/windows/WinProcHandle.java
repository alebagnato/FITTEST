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
package org.fruit.alayer.windows;

import org.fruit.alayer.devices.ProcessHandle;

public final class WinProcHandle implements ProcessHandle {
	private final long pid;
	public WinProcHandle(long pid){ this.pid = pid;	}
	public void kill() { WinProcess.killProcess(pid); }
	public boolean isRunning() { return WinProcess.isRunning(pid); }
	public String name() {
		try{
			return WinProcess.procName(pid);
		}catch(WinApiException wae){
			return null;
		}
	}
	public long pid() { return pid; }
}
