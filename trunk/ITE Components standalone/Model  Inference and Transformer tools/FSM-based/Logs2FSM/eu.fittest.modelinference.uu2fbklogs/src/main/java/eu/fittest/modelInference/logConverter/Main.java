/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.modelInference.logConverter;

import java.io.File;
import java.util.Iterator;

import eu.fittest.modelInference.logConverter.Utility;

/**
 * Main to use the converter
 * 
 * @author Alessandro Marchetto
 *
 */
public class Main {
	Converter uu2fbkLogs=new Converter();
	Utility utils=new Utility();
	
	/**
	 * @param String inputfolderpath: path of the folder that contains the xml logs
	 * @param String prefix_outputFileName: prefix of the name for the generated log files
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Main m=new Main();
		if (args!=null){
			if (args.length==2) {
				m.convert(args[0],args[0]);
			}else m.convert("","");
		}else {
			m.convert("","");
		}
	}

	void convert(String inputfolderpath,String prefix_outputFileName){
		String filePath="input"+System.getProperty("file.separator")+"lastuu"+System.getProperty("file.separator"); 
		String outputFileName="log_";
		
		//String filePath="input"+System.getProperty("file.separator")+"uu"+System.getProperty("file.separator"); 
		//String outputFileName="log_";
		
		
		String tmp="";
		
		if (inputfolderpath.equalsIgnoreCase("")) inputfolderpath=filePath;
		//if (prefix_outputFileName.equalsIgnoreCase("")) prefix_outputFileName=outputFileName;
		//else {
			tmp=prefix_outputFileName;
			prefix_outputFileName=outputFileName+tmp;
		//}
		
		File[] filelist=utils.getFileList(inputfolderpath);
		for (int i = 0; i < filelist.length; i++) {
			if (filelist[i].isFile()) uu2fbkLogs.convert(filelist[i],prefix_outputFileName+"_"+i);
		}
		
	}
}
