package eu.fittest.fbk.efsm2ct.plugin.tool;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import eu.fittest.fbk.efsm2ct.plugin.ConfigurationFactory;
import eu.fittest.fbk.efsm2ct.plugin.utils.ResourceUtils;
import eu.fittest.fbk.efsm2ct.tools.evosuite.LogConsumer;
import eu.fittest.fbk.efsm2ct.tools.evosuite.ProcessSpawnException;
import eu.fittest.fbk.efsm2ct.tools.txl.TxlService;

public class AdaptationStep {

	
	public static int performAdaptation(File txlTransformationDir, String sessionName, IFolder evoTestsFolder,
			IFolder testSuitesDirectory, String txlTransformation, String additionalMethodFile) throws IOException,
			URISyntaxException, ExecutionException, CoreException,
			ProcessSpawnException {
		
		String packageNamePrefix = sessionName;
			
		// test case code


		File txlFile = new File(txlTransformationDir, txlTransformation);


		List<IResource> matchedTests = ResourceUtils.matchRecursively(evoTestsFolder,"^.*\\.java$");
		
		if (matchedTests.size() != 1) {
			throw new ExecutionException("mismatched number of java files in folder:"+evoTestsFolder);
		}

		//
		
		File inputFile = matchedTests.get(0).getLocation().toFile();

		File topOutputDir = testSuitesDirectory.getLocation().toFile();
		
		File outputDir = new File(topOutputDir, packageNamePrefix.replace('.', File.separatorChar));

		if (!outputDir.exists()) {
			outputDir.mkdirs();
		}

		String className = "JTest";
		
		File outputFile = new File(outputDir, className+".java");

		TxlService ts = new TxlService(txlFile);

		// File homeDir = new File(System.getProperty("user.home"));

		ts.setTxlHomePath(new File(ConfigurationFactory.getInstance().getTxlHomeDirectory()));

		ts.addLogConsumer(new LogConsumer() {

			@Override
			public void consume(char ch) {
				System.out.print(ch);

			}
		});

		ts.init();

		ts.init(inputFile, outputFile);

		ts.addCmd("-");
		ts.addCmd(className);

		File addMethsFile = new File(txlTransformationDir, additionalMethodFile);

		ts.addCmd(addMethsFile.getAbsolutePath());
		
			int status = ts.run();
		return status;
	}

	
}
