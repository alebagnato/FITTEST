package eu.fittest.modelInference.logConverter;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;

/**
 * Some generic utilities
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Utility {
	BufferedWriter outputFile = null;

	public boolean startFile(String fileName) {

		try {
			outputFile = new BufferedWriter(new FileWriter(fileName + ".txt"));
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

	/*
	 * It returns the list of files
	 */
	public File[] getFileList(String folderPath) {

		File folder = new File(folderPath);
		File[] listOfFiles = folder.listFiles();

		Vector<File> listTMP = new Vector<File>();
		String fname = "";

		for (int i = 0; i < listOfFiles.length; i++) {
			if (listOfFiles[i].isFile()) {
				fname = listOfFiles[i].getName();
				if (fname.endsWith(".xml")) {
					listTMP.add(listOfFiles[i]);
				}
			}
		}

		File[] listOfXMLFiles = new File[listTMP.size()];
		int index = 0;
		for (File file : listTMP) {
			listOfXMLFiles[index] = file;
			index++;
		}

		return listOfXMLFiles;
	}
}
