/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.xinputmining;


import java.io.File;

import org.junit.Ignore;
import org.junit.Test;

public class XinputMinerTest {
	
	private String projectRoot = System.getProperty("user.dir", "."); 
	private String testDataFolder = projectRoot + File.separator
			+ "src" + File.separator
			+ "test" + File.separator
			+ "resources" + File.separator
			+ "cutenews" + File.separator;

	
	@Ignore
	public void testWithLog() {
		String xinputFile = testDataFolder + "CuteNews.xml";
		String inputModel = testDataFolder + "CuteNews.fsm";
		String logFolder =  testDataFolder + "tmp";
		XinputMiner miner = new XinputMiner();
		miner.mine(inputModel, logFolder, xinputFile);
	}
	
	@Ignore
	public void testWithLog1() {
		String testDataFolder1 = projectRoot + File.separator
				+ "src" + File.separator
				+ "test" + File.separator
				+ "resources" + File.separator
				+ "data" + File.separator;
		String xinputFile = testDataFolder1 + "CuteNews.xml";
		String inputModel = testDataFolder + "CuteNews.fsm";
		
		XinputMiner miner = new XinputMiner();
		miner.mine(inputModel, testDataFolder1, xinputFile);
	}

	@Ignore
	public void testWithOutLog() {
		String xinputFile = testDataFolder + "cyclos.xml";
		String inputModel = testDataFolder + "cyclos.fsm";
		String logFolder =  null;
		XinputMiner miner = new XinputMiner();
		miner.mine(inputModel, logFolder, xinputFile);
	}

}
