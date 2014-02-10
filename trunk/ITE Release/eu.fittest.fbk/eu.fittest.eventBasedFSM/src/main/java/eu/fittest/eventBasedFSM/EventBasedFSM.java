package eu.fittest.eventBasedFSM;

import eu.fbk.se.fsm.ComputeModelFromTraces;
import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.FittestTrcReader;
import eu.fbk.se.fsm.TraceFileReader;
import eu.fbk.se.fsm.TrcReader;
import eu.fittest.FittestXMLReader;

public class EventBasedFSM {

	/**
	 * It generates an event-based FSM
	 * 
	 * @param dirTracesPath
	 *            path to the traces (default: ./input)
	 * @param outputFolderFile
	 *            the file to write the generated FSM (default: ./output.txt)
	 * @param traceFile_Extension
	 *            extension of (default: trc)
	 */
	static public void computeFSMfromTraces(String dirTracesPath,
			String outputFolderFile, String traceFile_Extension)
			throws Exception {

		String prefix = System.getProperty("user.dir")
				+ System.getProperty("file.separator");
		
		if (dirTracesPath.equals(""))
			dirTracesPath = prefix + System.getProperty("file.separator")
					+ "input";
		
		if (outputFolderFile.equals(""))
			outputFolderFile = prefix + "output.txt";
		if (traceFile_Extension.equals(""))
			traceFile_Extension = "trc";

		TraceFileReader traceReader;
		if ("xml".equals(traceFile_Extension)) 
			traceReader = new FittestXMLReader(); // flash based log
		else if ("trc".equals(traceFile_Extension))
			traceReader = new TrcReader();
		else {
			traceFile_Extension = "xml"; // assign again the xml extention so that the reader can find files 
			traceReader = new FittestTrcReader(); // generic ITE XML reader
		}
		
		ComputeModelFromTraces cm = new ComputeModelFromTraces(traceReader);
		FSM fsm;

		fsm = cm.readTraces(dirTracesPath, traceFile_Extension);
		fsm = cm.makeReadable(fsm);
		fsm.print(outputFolderFile);
		// fsm.print();

	}

	/**
	 * It generates a set of (event-based) FSMs optimizing their over/under
	 * approximation and size
	 * 
	 * @param dirTracesPath
	 *            path to the traces (default: ./input)
	 * @param outputFolderPath
	 *            path to write the generated FSMs (default: ./output)
	 * @param traceFile_Extension
	 *            extension of (default: trc)
	 * @param configfilePath
	 *            xml files to configure the parameters of the genetic algorithm
	 *            (default: no file is used)
	 * @param evoAlgType
	 *            it could be NSGA or GA (default: GA)
	 */
	/*
	 * static public void searchBasedFSMfromTracesBy(String dirTracesPath,
	 * String outputFolderPath,String traceFile_Extension, String
	 * configfilePath, String evoAlgType)throws Exception {
	 * System.err.println("Usage of SearchBasedFSM ...");
	 * 
	 * String
	 * prefix=System.getProperty("user.dir")+System.getProperty("file.separator"
	 * );
	 * 
	 * if (dirTracesPath.equals("")) dirTracesPath=prefix+"input"; //if
	 * (configfilePath.equals(""))
	 * configfilePath=prefix+"EvoAlgConfig"+System.getProperty
	 * ("file.separator")+"ga-params.xml"; if (evoAlgType.equals(""))
	 * evoAlgType="GA"; if (outputFolderPath.equals(""))
	 * outputFolderPath=prefix+"output"; if (traceFile_Extension.equals(""))
	 * traceFile_Extension="trc";
	 * 
	 * eu.fbk.se.fsm.SearchBasedFSM.run(dirTracesPath,configfilePath,evoAlgType,
	 * outputFolderPath,traceFile_Extension); }
	 */

	/**
	 * It generates an event-based FSM
	 * 
	 * @param dirTracesPath
	 *            path to the traces (default: ./input)
	 */
	static public void inferenceOfEventBasedFSM(String dirTracesPath)
			throws Exception {
		computeFSMfromTraces(dirTracesPath, "", "txt");
		// searchBasedFSMfromTracesBy(dirTracesPath,"","txt","","");
	}

	static public void main(String args[]) {
		try {
			// System.out.println(args.length);
			if ((args.length < 2) || (args.length > 6)) {
				System.out
						.println("Wrong argument list. Please, use one of the following:");
				System.out.println("complete [./input] [./output.txt] [trc]");
				System.out
						.println("optimized [./input] [./output.] [trc] [./EvoAlgConfig/ga-params.xml] [GA]");
				System.out.println("both [./input]");

			} else if (args.length == 1) {
				inferenceOfEventBasedFSM(args[0]);
			} else {
				if (args[0].equalsIgnoreCase("both")) {
					if (args.length == 2)
						inferenceOfEventBasedFSM(args[1]);
				} else if (args[0].equalsIgnoreCase("complete")) {
					if (args.length == 2)
						computeFSMfromTraces(args[1], "", "");
					else if (args.length == 3)
						computeFSMfromTraces(args[1], args[2], "");
					else if (args.length == 4)
						computeFSMfromTraces(args[1], args[2], args[3]);
					else
						computeFSMfromTraces("", "", "");
				} else if (args[0].equalsIgnoreCase("optimized")) {
					// if (args.length==6)
					// searchBasedFSMfromTracesBy(args[1],args[2],args[3],args[4],args[5]);
					// else searchBasedFSMfromTracesBy("","","","",""
					System.out.println("Feature removed");
				} else
					System.out.println("Wrong parameter values");
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}