package eu.fittest.selenium2fsm;

import java.io.File;

import eu.fbk.xinputmining.XinputMiner;
import eu.fittest.converter.LogConverterException;
import eu.fittest.converter.SeleniumHtml2ITELogConverter;
import eu.fittest.eventBasedFSM.EventBasedFSM;


public class FSMGenerator {

	public void generateFSM(String inputFolder, String outputFolder,
			String modelName) throws LogConverterException {

		// /////////////////////////////////////////////////
		// 1. convert selenium html test cases to ITE xml log format
		File outDir = new File(outputFolder);
		if (!outDir.exists()) {
			outDir.mkdirs();
		} else {
			// should warn user that exisiting files will be overitten
		}

		File outTmpDir = new File(outputFolder + File.separatorChar + "tmp");
		if (outTmpDir.exists()) {
			// empty this temporary directory fist
			outTmpDir.delete();
		}
		outTmpDir.mkdir();

		String outTmpFolder = outTmpDir.getAbsolutePath();

		SeleniumHtml2ITELogConverter seleniumConverter = new SeleniumHtml2ITELogConverter();
		seleniumConverter.convertAll(inputFolder, outTmpFolder);

		// 2. Infer model
		String outputModelName = outputFolder + File.separatorChar + modelName + ".fsm";
		try {
			String absTraceFolder = outTmpFolder + File.separator + "abstract";
			EventBasedFSM.computeFSMfromTraces(absTraceFolder, outputModelName, "xml");
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// 3. Generate xinput file
		XinputMiner xinputMiner = new XinputMiner();
		String concreteTraceFolder = outTmpFolder + File.separator + "concrete";

		String xinputFile = outputModelName.replace(".fsm", ".xml");
		xinputMiner.mine(outputModelName, concreteTraceFolder, xinputFile);

	}

	/**
	 * For testing purpose only, on validity checking is implemented.
	 * @param args
	 */
	public static void main(String[] args) {
		if (args.length == 3){
			FSMGenerator generator = new FSMGenerator();
			try {
				generator.generateFSM(args[0], args[1], args[2]);
			} catch (LogConverterException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else {
			System.out.println("Usage: FSMGenerator inputDir outputDir appName");
		}
	}
}
