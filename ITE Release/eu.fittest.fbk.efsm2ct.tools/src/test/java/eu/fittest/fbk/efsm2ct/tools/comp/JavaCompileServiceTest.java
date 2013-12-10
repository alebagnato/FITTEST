package eu.fittest.fbk.efsm2ct.tools.comp;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

public class JavaCompileServiceTest {

	@Test
	public void test() throws IOException {

		String sourceFile = "src/test/input";
		String targetDirectory = "target/output";

		File dir = new File(targetDirectory);

		dir.mkdirs();

		File s = new File(sourceFile);

		String absSourceFile = s.getAbsolutePath();

		JavaCompileService comp = new JavaCompileService(absSourceFile, targetDirectory);

		comp.run();
	}

}
