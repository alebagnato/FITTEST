package eu.fittest.modelInference.imu.test;

import eu.fittest.modelInference.fsmInference.fsm.Transition;

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
	
	public void writeInFile(Vector<TestCase> tcs, String outputDirName, String outputFileName_prefix){
			
			TestCase tc;
			BufferedWriter outF;
			String line="";
			Vector<Transition> transV;
				
			 for (int i = 0; i < tcs.size(); i++) {
				 tc=tcs.get(i);
				 if (i==0) {
					 outF=utils.createFile(outputDirName, outputFileName_prefix+"_"+i+".txt",false);
				 }
				 else {
					 outF=utils.createFile(outputDirName, outputFileName_prefix+"_"+i+".txt",true);
				 }
				 transV=tc.getTC();
				 if (transV!=null){
					 for (int z = 0;z < transV.size(); z++) {
						 line=":__:"+ transV.get(z).getTransitionContent()[0]+":__:[S"+transV.get(z).getIdStateTarget()+";__;]";
						 utils.writeInFile(outF, line);
					 }
				 }
				 if (outF!=null) utils.closeFile(outF);
			}
			 
			 
		     
		     
		 }
	
	public void writeInFiles(Vector<TestCase> tcs, String outputDirName, String outputFileName_prefix){
		
		TestCase tc;
		BufferedWriter outF;
		String line="";
		Vector<Transition> transV;
			
		 for (int i = 0; i < tcs.size(); i++) {
			 tc=tcs.get(i);
			
			 outF=utils.createANewFile(outputDirName, outputFileName_prefix+"_"+i+".txt");
			 if (outF==null) System.out.println("prova "+outputDirName+System.getProperty("file.separator")+outputFileName_prefix+"_"+i+".txt");
			 
			 transV=tc.getTC();
			 if (transV!=null){
				 for (int z = 0;z < transV.size(); z++) {
					 line=":__:"+ transV.get(z).getTransitionContent()[0]+":__:[S"+transV.get(z).getIdStateTarget()+";__;]";
					
					 utils.writeInFile(outF, line);
				 }
			 }
			 if (outF!=null) utils.closeFile(outF);
		}
		 
		 
	     
	     
	 }
	
	
	
}
