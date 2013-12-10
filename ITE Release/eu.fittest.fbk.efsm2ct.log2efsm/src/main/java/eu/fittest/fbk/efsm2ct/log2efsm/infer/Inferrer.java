package eu.fittest.fbk.efsm2ct.log2efsm.infer;

import java.io.File;
import java.util.logging.Logger;

import eu.fittest.fbk.efsm2ct.log2efsm.infer.formats.EfsmSerializer;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.formats.InspectorDescriptionSet;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.formats.MutatorDescriptionSet;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.formats.VelocityFsmtestDriverGenerator;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.Fsm;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.manager.FSMmanager;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.Utility;

/**
 * Entry point for FSM model inference
 * 
 * @author tiella
 * 
 */

public class Inferrer {

	private static Logger logger = Logger.getAnonymousLogger();

	private String packageName = null;
	private String className = null;
	private String mutatorsFile;

	private String inputFolder;
	private String outputFsmfileName;
	private String logFilenamePrefix;

	public Inferrer(String packageName, String className, String inputFolder, String outputFsmfileName, String mutatorsFileName, String logFilenamePrefix, String outputDriverDirectory) {

		this.packageName = packageName;
		this.className = className;
		this.inputFolder = inputFolder;
		this.mutatorsFile = mutatorsFileName;
		this.outputFsmfileName = outputFsmfileName;
		this.logFilenamePrefix = logFilenamePrefix;

	}

	public void infer() {
		try {

			FSMmanager fsmmanager = new FSMmanager();

			File[] filelist = Utility.getFileList(inputFolder, ".txt");

			logger.info("files read:" + filelist.length);

			Fsm fsm = fsmmanager.generateFSM(filelist, logFilenamePrefix, 0);

			clean(fsm);

			InspectorDescriptionSet ids = new InspectorDescriptionSet(fsm);

			MutatorDescriptionSet mds = new MutatorDescriptionSet();
			mds.init(mutatorsFile);

			EfsmSerializer efsmSerializer = new EfsmSerializer(packageName, className, mds, outputFsmfileName);

			efsmSerializer.export(fsm);

			logger.info("..FSM generated (" + outputFsmfileName + ")");
			logger.info(" - size FMS = " + fsm.transitions.size());

// RT: moved to another phase
//			VelocityFsmtestDriverGenerator fsmGen = new VelocityFsmtestDriverGenerator(packageName, className, mds, ids);
//			fsmGen.generate(outputDriverDirectory);

		} catch (Exception e) {
			System.out.println("Error in modelsInference");
			e.printStackTrace();
		}
	}

	/**
	 * inferred FSM needs some cleaning
	 * 
	 * @param fsm
	 */

	private void clean(Fsm fsm) {

		// TODO

	}

}
