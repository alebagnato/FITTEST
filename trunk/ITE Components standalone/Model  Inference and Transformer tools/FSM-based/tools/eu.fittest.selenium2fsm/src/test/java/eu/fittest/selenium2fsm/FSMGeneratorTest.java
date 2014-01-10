/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.selenium2fsm;


import java.io.File;

import org.junit.Test;

import eu.fittest.converter.LogConverterException;

public class FSMGeneratorTest {
	
	String projectRoot = System.getProperty("user.dir");
	
	@Test
	public void testModelGeneration() {
		String seleniumFolder = projectRoot 
				+ File.separator + "src"
				+ File.separator + "test"
				+ File.separator + "resources"
				+ File.separator + "selenium";
		
		String outputFolder = projectRoot 
				+ File.separator + "src"
				+ File.separator + "test"
				+ File.separator + "resources"
				+ File.separator + "output";
		
		FSMGenerator generator = new FSMGenerator();
		try {
			generator.generateFSM(seleniumFolder, outputFolder, "CuteNews");
		} catch (LogConverterException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

}
