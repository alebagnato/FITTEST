package eu.fittest.ucl.api;

import java.util.List;

import org.junit.Ignore;
import org.junit.Test;

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
		String logFolder = "/home/kiran/workspace/fittest/Documents/IR2.15-Flexstore-Continuous/experiment_data/Logs/fixed_ids/flexstorev3_deep_FBKserver/toTest";
		List<Body> logBodies = XMLUtils.loadXMLLog(logFolder, 4);
		
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
}
