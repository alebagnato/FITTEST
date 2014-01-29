package eu.fittest.fbk.efsm2ct.tools.rewrite;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TestFixer {

	private String input;

	private static Pattern removeImports = Pattern.compile("^(import eu\\.fittest\\..*)$", Pattern.MULTILINE);

	public TestFixer(String in) {

		input = in;

	}

	public TestFixer() {
		// nothing to do
	}

	public void removeImports() {

		Matcher m = removeImports.matcher(input);
		input = m.replaceAll("// $1");

	}

	public void fixit() {

		removeImports();

	}

	public void readInput(File file) throws IOException {

		FileReader fr = new FileReader(file);

		char[] buf = new char[(int) file.length()];

		fr.read(buf);

		fr.close();

		input = new String(buf);

	}

	public String getInput() {
		return input;
	}

}
