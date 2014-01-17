package eu.fittest.modelInference.fsmInference.modelInference;

import java.io.*;
import java.util.*;

import eu.fittest.modelInference.fsmInference.fsm.*;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class ConvertFSMXpr {

	BufferedWriter outputFile;

	Transitions transitions;
	Vector<String> transitionConverted;

	public ConvertFSMXpr(Transitions transitions) {
		this.transitions = transitions;
		transitionConverted = new Vector<String>();
	}

	public void convertTransitions(String fileName, String dirName, String ext) {
		Transition t;
		String str;

		if (!dirName.equals("")) {
			createDirectory(dirName);
			startFile(
					dirName + System.getProperty("file.separator") + fileName,
					ext);
		} else
			startFile(fileName, ext);

		for (int i = 0; i < transitions.size(); i++) {
			t = transitions.getTransitions().get(i);
			str = assembleLine(new Long(t.getIdStateSource()).toString(),
					new Long(t.getIdStateTarget()).toString(),
					t.getTransitionContent()[0]);
			if (!transitionConverted.contains(str)) {
				transitionConverted.add(str);
				addLine(str);
			}
		}
		emptyCloseFile();
	}

	Hashtable<String, String> labels = null;
	int indexOfLastLabel = 0;

	public void convertTransitions_readableLabels(String fileName,
			String dirName, String ext) {
		Transition t;
		String str;
		labels = new Hashtable<String, String>(); // <original label = state id,
													// readable_Label>

		// modifed by Cu
		if (dirName != null && !dirName.equals("")) {
			createDirectory(dirName);
			startFile(
					dirName + System.getProperty("file.separator") + fileName,
					ext);
		} else if (ext != null) {
			startFile(fileName, ext);
		} else
			startFile(fileName);

		String[] tmpLabs = null; // [0] source, [1] target
		indexOfLastLabel = 0;

		for (int i = 0; i < transitions.size(); i++) {
			t = transitions.getTransitions().get(i);

			tmpLabs = convertLabel(t);

			if (tmpLabs == null)
				str = assembleLine(new Long(t.getIdStateSource()).toString(),
						new Long(t.getIdStateTarget()).toString(),
						t.getTransitionContent()[0]);
			str = assembleLine(tmpLabs[0], tmpLabs[1],
					t.getTransitionContent()[0]);

			if (!transitionConverted.contains(str)) {
				transitionConverted.add(str);
				addLine(str);
			}
		}

		emptyCloseFile();
	}

	private String[] convertLabel(Transition t) {
		String[] tmpLab = new String[2];

		if (labels.containsKey(new Long(t.getIdStateSource()).toString())) {
			tmpLab[0] = labels.get(new Long(t.getIdStateSource()).toString());

		} else {
			indexOfLastLabel++;
			tmpLab[0] = "" + indexOfLastLabel;
			labels.put(new Long(t.getIdStateSource()).toString(), tmpLab[0]);
		}

		if (labels.containsKey(new Long(t.getIdStateTarget()).toString())) {
			tmpLab[1] = labels.get(new Long(t.getIdStateTarget()).toString());

		} else {
			indexOfLastLabel++;
			tmpLab[1] = "" + indexOfLastLabel;
			labels.put(new Long(t.getIdStateTarget()).toString(), tmpLab[1]);
		}

		return tmpLab;
	}

	private boolean startFile(String fileName, String ext) {
		try {
			if (ext.equals("")) {
				if (ext.startsWith("."))
					outputFile = new BufferedWriter(new FileWriter(fileName
							+ ext));
				else
					outputFile = new BufferedWriter(new FileWriter(fileName
							+ "." + ext));
			} else
				outputFile = new BufferedWriter(new FileWriter(fileName
						+ ".fsm"));
			return true;
		} catch (IOException e) {
			return false;
		}
	}

	/**
	 * Just one param for the output file
	 * 
	 * @author cunduy
	 * @param fileName
	 * @param ext
	 * @return
	 */
	private boolean startFile(String fileFullName) {
		try {
			outputFile = new BufferedWriter(new FileWriter(fileFullName));
			return true;
		} catch (IOException e) {
			return false;
		}
	}

	private boolean emptyCloseFile() {
		try {
			outputFile.close();
			return true;
		} catch (IOException e) {
			return false;
		}
	}

	private boolean closeFile() {
		try {
			outputFile.write("}\r\n");
			outputFile.close();
			return true;
		} catch (IOException e) {
			return false;
		}
	}

	private String assembleLine(String source, String target, String event) {
		String str = "S" + source + " ->[" + event + "] S" + target + ";";
		return str;
	}

	private boolean addLine(String line) {
		try {
			outputFile.write(line + "\r\n");
			return true;
		} catch (IOException e) {
			return false;
		}
	}

	private boolean createDirectory(String dirName) {
		try {
			boolean success = false;
			File d = new File(dirName);
			if (d.exists() == false) {
				success = (d).mkdirs();
				if (!success) {
					return true;
				}
			}
			return false;
		} catch (Exception e) {
			return false;
		}
	}

}
