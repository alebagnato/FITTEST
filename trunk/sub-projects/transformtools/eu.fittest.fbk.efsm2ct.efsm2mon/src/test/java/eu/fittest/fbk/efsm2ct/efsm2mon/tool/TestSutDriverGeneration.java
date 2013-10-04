package eu.fittest.fbk.efsm2ct.efsm2mon.tool;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.parser.FsmParser;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;

public class TestSutDriverGeneration {
	
	private static String fsmFilePath = "src/test/input/efsms/Flexstore_vold.efsm";
	private static String packageName = "cart";
	private static Model m;
	
	@Before
	public void initModel() throws FileNotFoundException, FsmTesterException {
		
		m = FsmParser.parse(new FileReader(fsmFilePath));

		StringBuilder annotation = new StringBuilder();

		if (!m.checkSemantic(annotation)) {
			fail("semantic errors in input:"+annotation);
		}
	}

	@Test
	public void testRun() throws FsmTesterException, IOException {

		String destDirName = "target/output/gensrc";
		boolean onlyOut = false;
		boolean hasRunControl = true;
		String tmplFileName = "eu/fittest/fbk/efsm2ct/efsm2mon/vm/fsm-int.vm";

		Main.runJava(m, packageName, "", destDirName, onlyOut, hasRunControl, tmplFileName);
		
	}

}
