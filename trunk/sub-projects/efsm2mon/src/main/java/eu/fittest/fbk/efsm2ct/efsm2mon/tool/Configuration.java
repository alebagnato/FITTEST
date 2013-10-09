/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.fbk.efsm2ct.efsm2mon.tool;

/**
 * 
 * @author tiella
 */
public class Configuration {

	private static Configuration instance;

	public static Configuration getInstance() {
		if (instance == null) {

			instance = new Configuration();

		}

		return instance;
	}

	public String getCompilerDestDir() {
		return System.getProperty("fsmtester.compiler.dest.dir");
	}

}
