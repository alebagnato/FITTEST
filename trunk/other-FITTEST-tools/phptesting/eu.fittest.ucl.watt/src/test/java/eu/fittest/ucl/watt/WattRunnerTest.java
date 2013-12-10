package eu.fittest.ucl.watt;

import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Test;

public class WattRunnerTest {

	@Test
	public void testWatt() {
		File scriptFolder = new File("/home/kiran/workspace/fittest/watt/src/main/resources/wattapp");
		Database db = new Database("root", "mypass");
		WattRunner runner = new WattRunner(db, scriptFolder);
		assertTrue(runner.runWatt("http://qt-project.org/wiki/Building_Qt_5_from_Git", "", ""));
	}
}
