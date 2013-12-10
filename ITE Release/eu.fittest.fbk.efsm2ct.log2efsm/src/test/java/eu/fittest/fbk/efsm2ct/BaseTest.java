package eu.fittest.fbk.efsm2ct;

import java.io.File;

public class BaseTest {

	protected void createDirectory(String dir) {

		File targetDir = new File(dir);

		if (!targetDir.exists()) {
			targetDir.mkdirs();
		}

	}

}
