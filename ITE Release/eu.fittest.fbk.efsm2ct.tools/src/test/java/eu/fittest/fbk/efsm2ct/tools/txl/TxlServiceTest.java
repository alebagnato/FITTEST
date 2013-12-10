package eu.fittest.fbk.efsm2ct.tools.txl;

import static org.junit.Assert.*;

import java.io.File;

import org.junit.Test;

import eu.fittest.fbk.efsm2ct.tools.evosuite.LogConsumer;
import eu.fittest.fbk.efsm2ct.tools.evosuite.ProcessSpawnException;

public class TxlServiceTest {

	// @Test
	public void testSimple() throws ProcessSpawnException {

		File txlDir = new File("src/test/txl/a");

		File txlFile = new File(txlDir, "ident.txl");
		File inputFile = new File(txlDir, "in.ident");

		File outputDir = new File("target/output/txl/a");

		if (!outputDir.exists()) {
			outputDir.mkdirs();
		}

		File outputFile = new File(outputDir, "out.ident");

		TxlService ts = new TxlService(txlFile);

		
		ts.addLogConsumer(new LogConsumer() {
			
			@Override
			public void consume(char ch) {
				System.out.print(ch);
				
			}
		});

		ts.init();

		ts.init(inputFile, outputFile);
		
		ts.run();

	}
	
	@Test
	public void testEvo() throws ProcessSpawnException {

		File txlDir = new File("src/test/txl/evo");

		File txlFile = new File(txlDir, "remove_try_catch.txl");
		
		File inputDir = new File("src/test/input/evo");
		File inputFile = new File(inputDir, "TestTestFlexstore.java");

		File outputDir = new File("target/output/txl/evo");

		if (!outputDir.exists()) {
			outputDir.mkdirs();
		}

		File outputFile = new File(outputDir, "JTestFlexstore.java");

		TxlService ts = new TxlService(txlFile);
		
		File homeDir = new File(System.getProperty("user.home"));
		
		ts.setTxlHomePath(new File(homeDir, "programmi/txl10.6.linux64"));
		
		ts.addLogConsumer(new LogConsumer() {
			
			@Override
			public void consume(char ch) {
				System.out.print(ch);
				
			}
		});

		ts.init();

		ts.init(inputFile, outputFile);
		
		ts.addCmd("-");
		ts.addCmd("JTestFlexstore");
		
		File addMethsFile = new File(txlDir, "additional_methods.java");
		
		ts.addCmd(addMethsFile.getAbsolutePath());
		ts.run();

	}

}
