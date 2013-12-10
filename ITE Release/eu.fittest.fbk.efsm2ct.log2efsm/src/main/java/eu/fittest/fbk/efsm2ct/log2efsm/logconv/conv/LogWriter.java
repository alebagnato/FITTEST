package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * helper class to write a sequence of lines into a file TODO: remove sooner or
 * later (RT)
 */

public class LogWriter {

	BufferedWriter outputFile = null;

	// TODO remove try/catch throw exception instead

	public boolean startFile(File file) {

		try {
			outputFile = new BufferedWriter(new FileWriter(file));
			return true;

		} catch (IOException e) {
			return false;
		}

	}

	public boolean closeFile() {

		try {

			outputFile.close();
			return true;

		} catch (IOException e) {
			return false;
		}

	}

	public boolean writeLine(String line) {

		try {

			outputFile.write(line + "\n");
			return true;

		} catch (IOException e) {
			return false;
		}

	}

}
