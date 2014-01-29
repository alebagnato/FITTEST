package eu.fittest.ucl.api;

import java.util.List;

import org.junit.Ignore;
import org.junit.Test;

import daikon.Daikon;
import eu.fbk.xinputmining.XMLUtils;
import eu.fittest.itelog.Body;

public class ModelInferenceTest {
		
	@Ignore
	public void testInference() {
		String logFolder = "/Users/kiranlakhotia/Documents/workspace/fittest/UCL/workspace/eu.fittest.ucl.autoabs/traces/itelogs";
		List<Body> logBodies = XMLUtils.loadXMLLog(logFolder, 1);
		
		StateBasedModelInference mi = new StateBasedModelInference();
		
		mi.registerListener(new ModelInferenceListener()
		{

			public void onStart() {
				System.out.println("on staaaart");
				
			}

			public void progress(double percent, String message) {
				System.out.println("on progress:" + percent + "," + message);
				
			}

			public void onStop() {
				System.out.println("on stooop");
				
			}
			
		});
		
		ModelInferenceParameters config = new ModelInferenceParameters();
		config.setMaximumGenerations(3);
		mi.startInference(logBodies, null, null, config, true);
	}
	
	@Ignore
	public void testArrayOverflow() {
		String logFolder = "/home/kiran/workspace/fittest/UCL/workspace/eu.fittest.ucl.autoabs/traces/bug/advanced";
		List<Body> logBodies = XMLUtils.loadXMLLog(logFolder, 1);
		
		StateBasedModelInference mi = new StateBasedModelInference();
		
		mi.registerListener(new ModelInferenceListener()
		{

			public void onStart() {
				System.out.println("on staaaart");
				
			}

			public void progress(double percent, String message) {
				System.out.println("on progress:" + percent + "," + message);
				
			}

			public void onStop() {
				System.out.println("on stooop");
				
			}
			
		});
		
		ModelInferenceParameters config = new ModelInferenceParameters();
		config.setMaximumGenerations(3);
		mi.startInference(logBodies, null, null, config, true);
	}
	
	@Ignore
	public void testException() {
		String logFolder = "/home/kiran/workspace/fittest/UCL/workspace/eu.fittest.ucl.autoabs/traces/bug/example1";
		List<Body> logBodies = XMLUtils.loadXMLLog(logFolder, 3);
		
		StateBasedModelInference mi = new StateBasedModelInference();
		
		mi.registerListener(new ModelInferenceListener()
		{

			public void onStart() {
				System.out.println("on staaaart");
				
			}

			public void progress(double percent, String message) {
				System.out.println("on progress:" + percent + "," + message);
				
			}

			public void onStop() {
				System.out.println("on stooop");
				
			}
			
		});
		
		ModelInferenceParameters config = new ModelInferenceParameters();
		config.setMaximumGenerations(3);
		mi.startInference(logBodies, null, null, config, true);
	}
	
	@Test
	public void testITELogs() {
		String logFolder = "/home/kiran/workspace/ucl/fittest/eu.fittest.ucl.autoabs/traces/flexstore3_fsm_statebased/Testing sessions/FITTESTAgent-8156dc9b-25b5-49db-a172-631d54c08c79/eu.fittest.LoggingFittestLogger-75c4c434-5864-4991-b8da-3b49b0ff283c";
		List<Body> logBodies = XMLUtils.loadXMLLog(logFolder, 1);
		
		StateBasedModelInference mi = new StateBasedModelInference();
		
		mi.registerListener(new ModelInferenceListener()
		{

			public void onStart() {
				System.out.println("on staaaart");
				
			}

			public void progress(double percent, String message) {
				System.out.println("on progress:" + percent + "," + message);
				
			}

			public void onStop() {
				System.out.println("on stooop");
				
			}
			
		});
		
		ModelInferenceParameters config = new ModelInferenceParameters();
		config.setMaximumGenerations(3);
		mi.startInference(logBodies, null, null, config, true);
	}
	
	@Ignore
	public void testDaikon() {
		String[] args = {"-o", "/home/kiran/workspace/ucl/fittest/eu.fittest.ucl.autoabs/out/daikon/test.ing.gz", 
				"--nohierarchy", 
				"/home/kiran/workspace/ucl/fittest/eu.fittest.ucl.autoabs/out/daikon/cluster_12.decls",
				"/home/kiran/workspace/ucl/fittest/eu.fittest.ucl.autoabs/out/daikon/cluster_12.dtrace"};
		Daikon.cleanup();
		//infer invariants
		Daikon.mainHelper(args);
	}
}
