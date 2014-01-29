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
