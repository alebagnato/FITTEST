package eu.fittest.fbk.efsm2ct.log2efsm.logconv;

import java.io.File;
import java.util.Arrays;
import java.util.logging.Logger;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.ModelBasedConverter;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.Utility;

/**
 * Main to use the converter
 * 
 * @author Alessandro Marchetto, tiella
 * 
 */
public class Main {

	Logger logger = Logger.getAnonymousLogger();

//	private ModelBasedConverter mbConvert = new ModelBasedConverter();

	/**
	 * @param String
	 *            inputfolderpath: path of the folder that contains the xml logs
	 * @param String
	 *            prefix_outputFileName: prefix of the name for the generated
	 *            log files
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

		// System.out.println(System.getProperty("user.dir"));

		Main m = new Main();

		m.run(args);
	}

	private void run(String[] args) {

		logger.info("working directory:" + System.getProperty("user.dir"));

		int argc = 0;
		
		String filterFileName = null;

		// get options
		while (args.length > argc && args[argc].startsWith("-")) {

			String arg = args[argc];

			if (arg.equals("-h")) {

				usage();
				return;

			} else if (arg.equals("-f")) {

				filterFileName = args[argc+1];
				argc++;


			} else {
				System.err.println("unknown option: " + arg);
				usage();
				return;
			}

			// argc++;

		}

		// get directory which files have to be read from
		File[] fileList = null;
		String inputFolderPath = null;

		if (args.length > argc) {

			inputFolderPath = args[argc];
			argc++;

			logger.info("reading logs from:" + inputFolderPath);
			fileList = Utility.getFileList(inputFolderPath, ".xml");

		} else {

			System.err.println("input directory missed, in arguments: " + Arrays.toString(args));
			usage();
			System.exit(1);
		}

		// get output prefix

		String outputFileNamePrefix = null;

		if (args.length > argc) {

			outputFileNamePrefix = args[argc];
			argc++;

		} else {
			System.err.println("output prefix missed, in arguments: " + Arrays.toString(args));
			usage();
			System.exit(1);
		}

		// get target directory prefix

		String targetDirectoryPath = null;

		if (args.length > argc) {

			targetDirectoryPath = args[argc];
			argc++;

		} else {
			System.err.println("target directory file missed, in arguments: " + Arrays.toString(args));
			usage();
			System.exit(1);
		}

		File targetDirectory = new File(targetDirectoryPath);
		File mutatorsDescFile = new File(targetDirectory, outputFileNamePrefix + ".mut");

		File filterFile = null;
		
		if (filterFileName != null) {
			filterFile = new File(filterFileName);
		}
		
		Converter conv = new Converter();
		
		try {
			conv.run(fileList, mutatorsDescFile, outputFileNamePrefix, targetDirectory, filterFile);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			System.exit(1);
		}

	}

	private static void usage() {
		System.err.println("usage: <src dir> <output prefix> <target dir>");

	}

}
