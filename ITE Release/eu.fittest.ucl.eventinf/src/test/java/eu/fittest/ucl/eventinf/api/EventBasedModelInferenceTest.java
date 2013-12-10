package eu.fittest.ucl.eventinf.api;

import java.util.List;

import org.junit.Test;
import org.junit.Ignore;

import eu.fbk.xinputmining.XMLUtils;
import eu.fittest.itelog.Body;
import eu.fittest.ucl.api.ModelInferenceListener;

public class EventBasedModelInferenceTest {

	@Ignore
	public void testEventInference() {
		String logFolder = "/Users/kiranlakhotia/Documents/workspace/fittest/UCL/workspace/eu.fittest.ucl.autoabs/traces/itelogs";
		List<Body> logBodies = XMLUtils.loadXMLLog(logFolder, 3);
		
		EventBasedModelInference mi = new EventBasedModelInference();
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
		config.setMaxGenerations(3);
		config.setPopulationSize(20);
		config.setMaxFSMSize(1000);
		mi.startInference(logBodies, null, config, true);
	}
	
	@Test
	public void testStackOverflow() {
		String logFolder = "/home/kiran/workspace/fittest/UCL/workspace/eu.fittest.ucl.autoabs/traces/bug/example1";
		List<Body> logBodies = XMLUtils.loadXMLLog(logFolder, 3);
		
		EventBasedModelInference mi = new EventBasedModelInference();
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
		config.setMaxGenerations(3);
		config.setPopulationSize(20);
		config.setMaxFSMSize(1000);
		mi.startInference(logBodies, null, config, true);
	}
}
