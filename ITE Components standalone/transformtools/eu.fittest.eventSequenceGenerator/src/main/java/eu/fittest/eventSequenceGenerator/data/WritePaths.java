/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.eventSequenceGenerator.data;

import java.io.BufferedWriter;
import java.util.Vector;

import eu.fittest.modelInference.imu.utility.Utility;

/**
*
* @author Alessandro
*
*/
public class WritePaths {
	
	Utility utils=new Utility();
	BufferedWriter outF;

	public void printPaths(Vector<Path> paths, String dirOutput, String fileName) {
		
		if (!utils.createDirectory(dirOutput)){
			utils.emptyDirectory(dirOutput);
		}
		
		int i=0;
		String line="";
		
		for (Path path: paths) {
			 i++;
			 outF=utils.createANewFile(dirOutput, fileName+"_"+i+".txt");

			for (Edge e: path.getEdges()) {
				//line=":__:"+e.getEvent()+":__:[;__;]";
				line=e.getEvent();
				utils.writeInFile(outF, line);
			}
			
			if (outF!=null) utils.closeFile(outF);		
			
		}
		System.out.println("Size = "+paths.size());
	}

	
}
