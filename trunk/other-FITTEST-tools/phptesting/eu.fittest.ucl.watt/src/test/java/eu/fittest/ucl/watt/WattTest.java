package eu.fittest.ucl.watt;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.List;

import org.junit.Test;

import eu.fittest.ucl.watt.api.Watt;
import eu.fittest.ucl.watt.api.WattFormData;
import eu.fittest.ucl.watt.api.WattListener;

public class WattTest {
	
	public static boolean testStarted = false;
	public static boolean testStopped = false;
	
	public static class TestListener implements WattListener {

		public void onStart() {
			testStarted = true;
		}

		public void onStop() {
			testStopped = true;
		}
		
	}
	
	@Test
	public void testWatt() {
		File scriptFolder = new File("/home/kiran/workspace/fittest/watt/src/main/resources/wattapp");
		Watt w = new Watt();
		w.registerListener(new TestListener());
		w.start(scriptFolder, "http://qt-project.org/wiki/Building_Qt_5_from_Git", "", "",
				"root", "mypass");
		assertTrue(testStarted);
		assertTrue(testStopped);
		List<WattFormData> data = w.getAllSubmittedFormData();
		assertNotNull(data);
		for(WattFormData d : data) {
			System.out.println(d.getInputName() + " = " + d.getInputValue());
		}
	}
}
