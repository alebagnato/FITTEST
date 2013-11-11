package eu.fittest.fbk.efsm2ct.log2efsm.infer;

import java.util.logging.Logger;

/**
 * Main - it starts the model inference
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Main {

	private static Logger logger = Logger.getAnonymousLogger();

	private String packageName = "eu.fittest.sbtest.application.flexstore";
	private String className = "Flexstore";
	private String mutatorsFile = "/tmp/mutators.txt";

	private String inputFolder;
	private String outputFsmfileName;
	private String logFilenamePrefix;

	private String outputDriverDirectory;

	/**
	 * @param args
	 *            : [0] folderPath of logs , [1] name of the final FSM, [2]
	 *            extension of the output file (e.g., .dot, .fsm)
	 */
	public static void main(String[] args) {

		Main m = new Main();

		if (args.length > 0 && args[0].equals("-h")) {

			usage();

		} else {

			m.run(args);

		}
	}

	private static void usage() {
		System.out.println("usage: java -cp ... ... -m <file> -p <package> -c <class> -d <driver's output dir> <inputFolder> <outputFsmfileName>");
		System.out.println("\t-m filename containing mutators' definitions\n" + "\t-p EFSM's package name for the generated model\n" + "\t-c EFSM's name for the generated model\n"
				+ "\t-d driver's output directory\n" + "\t <inputFolder>\n" + "\t <logFilenamePrefix>" + "\t <outputFsmfileName>\n");

	}

	void run(String[] args) {

		int argc = 0;

		while (args.length - argc > 0 && args[argc].startsWith("-")) {

			String arg = args[argc];
			argc++;

			if (arg.equals("-m")) {

				mutatorsFile = args[argc];
				argc++;

			} else if (arg.equals("-p")) {

				packageName = args[argc];
				argc++;

			} else if (arg.equals("-c")) {

				className = args[argc];
				argc++;

			} else if (arg.equals("-d")) {

				outputDriverDirectory = args[argc];
				argc++;

			}

		}

		if (args.length - argc == 3) {

			inputFolder = args[argc];
			logFilenamePrefix = args[argc + 1];
			outputFsmfileName = args[argc + 2];

			Inferrer inferrer = new Inferrer(packageName, className, inputFolder, outputFsmfileName, mutatorsFile, logFilenamePrefix, outputDriverDirectory);
			inferrer.infer();

		} else {
			usage();
			System.exit(1);
		}

	}

}
