package eu.fittest.fbk.efsm2ct.log2efsm;

import java.io.File;

import org.junit.Assert;
import org.junit.Test;

import eu.fittest.fbk.efsm2ct.BaseTest;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.Main;

public class Test02InferMain extends BaseTest {

	// @Test
	public void testInfer_vold() {

		String dir = "target/output/gensrc/vold";

		createDirectory(dir);

		Main.main(new String[] { 
				"-m", "target/output/fsm_old/flexstore.mut", 
				"-p", "testflexstore", 
				"-c", "TestFlexstore", 
				"-d", dir, "target/output/fsm_old", 
				"flexstore_", "target/output/fsm_old/flexstore.efsm" 
				});

	}
	
        // @Test
	public void testInfer_v3() {

		String dir = "target/output/gensrc/v3";

		createDirectory(dir);

		Main.main(new String[] { 
				"-m", "target/output/fsm_v3/flexstore.mut", 
				"-p", "testflexstore", 
				"-c", "TestFlexstore", 
				"-d", dir, 
				"target/output/fsm_v3",	"flexstore_", "target/output/fsm_v3/flexstore.efsm" 
				});
		
		File output = new File("target/output/fsm_v3/flexstore.efsm");
		
		Assert.assertTrue(output.exists());

	}
	
	// @Test
		public void testInfer_5898() {

			String dir = "target/output/gensrc/v_5898";

			createDirectory(dir);

			Main.main(new String[] { 
					"-m", "target/output/fsm_5898/flexstore.mut", 
					"-p", "testflexstore", 
					"-c", "TestFlexstore", 
					"-d", dir, 
					"target/output/fsm_5898",	"flexstore_", "target/output/fsm_5898/flexstore.efsm" 
					});
			
			File output = new File("target/output/fsm_5898/flexstore.efsm");
			
			Assert.assertTrue(output.exists());

		}
	
	@Test
	public void testInfer_5898_1() {

		String dir = "target/output/gensrc/v_5898_1";

		createDirectory(dir);

		Main.main(new String[] { 
				"-m", "target/output/fsm_5898_1/flexstore.mut", 
				"-p", "testflexstore", 
				"-c", "TestFlexstore", 
				"-d", dir, 
				"target/output/fsm_5898_1",	"flexstore_", "target/output/fsm_5898_1/flexstore.efsm" 
				});
		
		File output = new File("target/output/fsm_5898_1/flexstore.efsm");
		
		Assert.assertTrue(output.exists());

	}

}
