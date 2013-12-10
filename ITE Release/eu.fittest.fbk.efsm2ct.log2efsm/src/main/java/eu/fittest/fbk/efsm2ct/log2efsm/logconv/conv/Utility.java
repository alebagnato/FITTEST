package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import java.io.File;
import java.util.Vector;

/**
 * Some generic utilities
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Utility {

	private static LogWriter logWriter = new LogWriter();

	private Utility() {
	}

	/*
	 * It returns the list of files
	 */
	public static File[] getFileList(String folderPath, String ext) {

		File folder = new File(folderPath);
		File[] listOfFiles = folder.listFiles();

		Vector<File> listTMP = new Vector<File>();
		String fname = "";

		for (int i = 0; i < listOfFiles.length; i++) {
			if (listOfFiles[i].isFile()) {
				fname = listOfFiles[i].getName();
				if (fname.endsWith(ext)) {
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

	public static boolean startFile(File file) {
		return logWriter.startFile(file);
	}

	public static boolean closeFile() {
		return logWriter.closeFile();
	}

	public static boolean writeLine(String line) {
		return logWriter.writeLine(line);
	}

}
