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
package org.fruit.example.windows;

import java.io.File;
import org.fruit.alayer.State;
import org.fruit.alayer.SUT;
import org.fruit.alayer.Widget;
import org.fruit.alayer.Roles;
import org.fruit.Util;
import org.fruit.alayer.windows.UIAStateBuilder;
import org.fruit.alayer.windows.WinProcess;

import static org.fruit.alayer.Tags.*;

public class PrintWidgetTree {
	
	public static void main(String[] args){
	
		if(args.length < 1 || !new File(args[0]).exists()){
			System.out.println("Invalid command line arguments!");
			return;
		}
				
		SUT system = WinProcess.fromExecutable(args[0]); // run the given executable
		
		Util.pause(5);
		State state = new UIAStateBuilder().apply(system);   // get the system's current state

		// print the role of each widget and a short description
		for(Widget widget : state){    
			// indent
			for(int i = 0; i < Util.depth(widget); i++)  
				System.out.print("  ");

			// print widget info
			System.out.printf("%s  %s\n", 
					widget.get(Role, Roles.Widget), 
					widget.get(Desc, "<desc unavailable>"));
		}
	
		system.stop();              // shut down the system
	}
}
