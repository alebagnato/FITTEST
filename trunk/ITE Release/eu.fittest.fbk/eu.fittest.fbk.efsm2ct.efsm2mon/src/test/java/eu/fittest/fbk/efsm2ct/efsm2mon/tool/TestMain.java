package eu.fittest.fbk.efsm2ct.efsm2mon.tool;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.Main.OutputType;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.parser.FsmParser;

public class TestMain {

	private static String fsmFilePath = "src/test/input/efsms/Cart.efsm";
	private static String packageName = "cart";
	private Model m;

	@Before
	public void initModel() throws FileNotFoundException, FsmTesterException {

		m = parseModel(fsmFilePath);

	}

	private Model parseModel(String file) throws FileNotFoundException,
			FsmTesterException {

		Model m = FsmParser.parse(new FileReader(file));

		StringBuilder annotation = new StringBuilder();

		if (!m.checkSemantic(annotation)) {
			fail("semantic errors in input:" + annotation);
		}
		return m;
	}

	// @Test
	public void testMain() {
		Main.usage();
	}

	// @Test
	public void testRunJava() throws FsmTesterException, IOException {

		String destDirName = "target/output/gensrc";
		boolean onlyOut = false;
		boolean hasRunControl = true;
		String tmplFileName = "eu/fittest/fbk/efsm2ct/efsm2mon/vm/fsm-sing-int.vm";

		Main.runJava(m, packageName, "Test", destDirName, onlyOut,
				hasRunControl, tmplFileName);

	}

	@Test
	public void testRunJava_5898() throws FsmTesterException, IOException {

		String destDirName = "target/output/gensrc";
		boolean onlyOut = false;
		boolean hasRunControl = true;
		String tmplFileName = "eu/fittest/fbk/efsm2ct/efsm2mon/vm/fsm-sing-int.vm";

		
		Model m1 = parseModel("src/test/input/efsms/flexstore_5898.efsm");
		
		Main.runJava(m1, packageName, "Test", destDirName, onlyOut,
				hasRunControl, tmplFileName);

	}

	// @Test
	public void testRunDot() throws FsmTesterException, IOException {

		String destDirName = "target/output/dot";
		boolean onlyOut = false;
		OutputType otype = OutputType.DOT_FITTEST;

		File d = new File(destDirName);
		d.mkdirs();

		Model m = parseModel("src/test/input/efsms/flexstore_v3.efsm");

		Main.runDot(m, packageName, destDirName, onlyOut, otype);
	}

        
        @Test
	public void testRunDotSimplified() throws FsmTesterException, IOException {

		String destDirName = "target/output/dot";
		boolean onlyOut = false;
		OutputType otype = OutputType.GENERIC;

		File d = new File(destDirName);
		d.mkdirs();

		Model m = parseModel("src/test/input/efsms/flexstore_v3.efsm");
                
                String templateFileName = "eu/fittest/fbk/efsm2ct/plugin/vm/dot-fittest-simpl.vm";

		Main.run(m, packageName, new File(destDirName,"flexstore_v3_simp.dot"), onlyOut, templateFileName, null, false);
	}

        
	// @Test
	public void testRunGml() throws FsmTesterException, IOException {

		String destDirName = "target/output/gml";
		boolean onlyOut = false;
		OutputType otype = OutputType.GML_ALIAS;

		Main.runGml(m, packageName, destDirName, onlyOut, otype);
	}

}
