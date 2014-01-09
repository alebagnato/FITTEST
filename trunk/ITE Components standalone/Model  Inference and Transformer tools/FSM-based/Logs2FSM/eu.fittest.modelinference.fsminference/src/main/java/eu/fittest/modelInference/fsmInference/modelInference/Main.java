/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.modelInference.fsmInference.modelInference;

import java.io.File;

import eu.fittest.modelInference.fsmInference.fsm.*;
import eu.fittest.modelInference.fsmInference.config.Config_Launcher;
import eu.fittest.modelInference.fsmInference.manager.FSMmanager;

/**
 * Main - it starts the model inference
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Main {
	Config_Launcher cl = Config_Launcher.getInstace();

	/**
	 * @param args
	 *            : [0] folderPath of logs , [1] name of the final FSM, [2]
	 *            extension of the output file (e.g., .dot, .fsm)
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Main m = new Main();
		m.set(args);
	}

	void set(String[] args) {
		if (args.length == 3) {
			infer(args[0], args[1], args[2]);
		} else {
			cl.fsm2dotFileName_final = cl.fsm2dotFileName_prefix + "_"
					+ cl.outputDirName + "_FSMfinal";
			infer(cl.folderPath, cl.fsm2dotFileName_final, cl.ext);
		}
	}

	void infer(String inputFolder, String outputFsmfileName, String ext) {
		try {

			FSMmanager fsmmanager = new FSMmanager();
			File[] filelist = fsmmanager.getFilelist(inputFolder,
					cl.maxFilePemutations);

			Fsm fsm = fsmmanager.generateFSM(filelist, false, 0);

			System.out.println("..FSM generated (" + outputFsmfileName + ")");
			System.out.println(" - size FMS = " + fsm.transitions.size());

			fsmmanager.FSM2DOT(fsm, outputFsmfileName);
			fsmmanager.FSM2FsmXpr(fsm, outputFsmfileName, "", ext);

		} catch (Exception e) {
			System.out.println("Error in: modelsInference");
			e.printStackTrace();
		}
	}

}
