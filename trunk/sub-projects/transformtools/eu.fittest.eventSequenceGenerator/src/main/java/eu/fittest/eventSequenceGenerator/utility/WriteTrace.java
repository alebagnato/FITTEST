/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.eventSequenceGenerator.utility;

import eu.fittest.eventSequenceGenerator.data.Path;
import eu.fittest.modelInference.fsmInference.fsm.Transition;


import eu.fittest.modelInference.fsmInference.fsm.*;
import  eu.fittest.modelInference.imu.test.TestCase;
import eu.fittest.modelInference.imu.utility.Utility;

import java.io.BufferedWriter;
import java.util.Vector;

/**
 * 
 * 
 * @author Alessandro Marchetto
 *
 */
public class WriteTrace {
	Utility utils=new Utility();
	
	
	public void writeInFiles(Vector<TestCase> tcs, String outputDirName, String outputFileName_prefix, Fsm fsm){
		
		TestCase tc;
		BufferedWriter outF;
		String line="";
		Vector<Transition> transV;
		
		 String stateTmp="";
		 String t="";
		
		 for (int i = 0; i < tcs.size(); i++) {
			 tc=tcs.get(i);
			
			 outF=utils.createANewFile(outputDirName, outputFileName_prefix+"_"+i+".txt");
			 if (outF==null) System.out.println("try "+outputDirName+System.getProperty("file.separator")+outputFileName_prefix+"_"+i+".txt");
			 
			 transV=tc.getTC();
			 if (transV!=null){
				 for (int z = 0;z < transV.size(); z++) {
					 //line=":__:"+ transV.get(z).getTransitionContent()[0]+":__:[S"+transV.get(z).getIdStateTarget()+";__;]";
					 
					 t=transV.get(z).getTransitionContent()[0];
					 
					 
					 if (!t.equals("_initialization")){
						 
					 if (t.trim().startsWith("_")) t=t.trim().substring(1);
					 }
					 
					 stateTmp=UtilityTracer.convert( fsm.states.getStateById(transV.get(z).getIdStateSource()).getStateContent() );
					 
					 if (stateTmp.equals("start")) {
						 stateTmp="";			 
					 }
					 					 
					 //if ((stateTmp.equals("end"))||(stateTmp.equals("start"))) line=":__:"+t+":__:[;__;]";
					 
					 //line=":__:"+t+":__:["+stateTmp+";__;]"; //org
					 line=t;
									 
					 utils.writeInFile(outF, line);
					 //}
					 
				 }
			 }
			 if (outF!=null) utils.closeFile(outF);
		}
		 
		 
	     
	     
	 }
	
}
