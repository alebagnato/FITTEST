/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.modelInference.fsmInference.config;

/**
 * 
 * @author Alessandro Marchetto Configuration file
 */
public class Config_Launcher {

	private static Config_Launcher instance = null;

	private Config_Launcher() {
	}

	public static Config_Launcher getInstace() {
		if (instance == null) {
			instance = new Config_Launcher();
		}
		return instance;
	}

	public int X = 2;
	public int maxFilePemutations = 1;
	public int maxloop = 2;

	public boolean change = true;

	/**
	 * directory of the logs
	 */
	// public String
	// folderPath="example"+System.getProperty("file.separator")+"input";
	// to be used
	// public String folderPath="tudu_all"+System.getProperty("file.separator");
	public String folderPath = "input" + System.getProperty("file.separator")
			+ "tudu_actual" + System.getProperty("file.separator");

	//
	// public String
	// folderPath="exampleCart_lastUU"+System.getProperty("file.separator")+"input"+System.getProperty("file.separator")+"one1"+System.getProperty("file.separator");
	// public String
	// folderPath="exampleCart_lastUU"+System.getProperty("file.separator")+"input"+System.getProperty("file.separator")+"one2"+System.getProperty("file.separator");
	// public String
	// folderPath="exampleCart_lastUU"+System.getProperty("file.separator")+"input"+System.getProperty("file.separator")+"one3"+System.getProperty("file.separator");
	// public String
	// folderPath="exampleCart_lastUU"+System.getProperty("file.separator")+"input"+System.getProperty("file.separator")+"three"+System.getProperty("file.separator");
	//
	// public String folderPath="arthur"+System.getProperty("file.separator");
	// public String
	// folderPath="arthur2_rq1rq2"+System.getProperty("file.separator")+"input"+System.getProperty("file.separator");
	// public String
	// folderPath="input"+System.getProperty("file.separator")+"calibration2";
	// public String
	// folderPath="input"+System.getProperty("file.separator")+"calibration";
	// public String
	// folderPath="input"+System.getProperty("file.separator")+"esempio";
	// public String
	// folderPath="input"+System.getProperty("file.separator")+"cart-flex-ex1";
	// public String
	// folderPath="input"+System.getProperty("file.separator")+"oryx"+System.getProperty("file.separator")+"calibration2s3";
	// public String
	// folderPath="input"+System.getProperty("file.separator")+"pafm"+System.getProperty("file.separator")+"calibration2b";

	// output filename
	public String fsm2dotFileName_prefix = "tudu";
	// public String fsm2dotFileName_prefix="pafm-calibration";
	// public String fsm2dotFileName_prefix="cartFlex-example";
	// public String fsm2dotFileName_prefix="arthur2_javanio";

	public String ext = ".fsm";

	// NO CHANGE THE FOLLOW
	// ---------------------------------------------------------------------------------

	public String outputDirName = "output";

	public String generateTraceDirName = "traceSim";
	public String fsm2dotFileName_final = "";
	public String generateTraceFileName = fsm2dotFileName_final;

	public String getFolderPath() {
		return folderPath;
	}

	public String getFsm2dotFileName_prefix() {
		return fsm2dotFileName_prefix;
	}

	public int getMaxFilePemutations() {
		return maxFilePemutations;
	}

	public String getOutputDirName() {
		return outputDirName;
	}

	public int getX() {
		return X;
	}

	public int getMaxloop() {
		return maxloop;
	}

	public void setChange(boolean change) {
		this.change = change;
	}

	public void setFolderPath(String folderPath) {
		this.folderPath = folderPath;
	}

}
