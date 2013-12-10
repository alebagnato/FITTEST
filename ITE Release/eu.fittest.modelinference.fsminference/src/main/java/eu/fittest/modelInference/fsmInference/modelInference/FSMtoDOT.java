package eu.fittest.modelInference.fsmInference.modelInference;

import java.util.*;
import java.io.*;

import eu.fittest.modelInference.fsmInference.fsm.*;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class FSMtoDOT {

	BufferedWriter outputFile;

	Hashtable<String, String[]> nodes;
	Vector<String> edges;

	public boolean apply(Fsm fsm, String fileName, String graphName) {
		boolean result = false;
		result = startFile(fileName, graphName);
		result = getNodes(fsm);
		result = closeFile();
		return result;
	}

	public boolean getNodes(Fsm fsm) {
		nodes = new Hashtable<String, String[]>();
		edges = new Vector<String>();
		Vector<String[]> tmp;
		String[] stateBefore;
		String event;
		String[] stateAfter;

		int numberOfState = 0;
		String name = "";
		String a;
		String b;
		for (int i = 0; i < fsm.getNumberOfConvertedEdges(); i++) {
			tmp = fsm.getConvertedEdgesByIndex(i);
			stateBefore = tmp.get(0);
			event = tmp.get(1)[0];
			stateAfter = tmp.get(2);

			if (!isInNodes(stateBefore)) {
				numberOfState = numberOfState + 1;
				name = "S" + numberOfState;
				nodes.put(name, stateBefore);
				addLine(assembleNodeLine(name, stateBefore));
			}
			if (!isInNodes(stateAfter)) {
				numberOfState = numberOfState + 1;
				name = "S" + numberOfState;
				nodes.put(name, stateAfter);
				addLine(assembleNodeLine(name, stateAfter));
			}

			a = getStateNameByContent(stateBefore);
			b = getStateNameByContent(stateAfter);

			if ((a != null) && (b != null)) {
				if (!edges.contains(a + b + event)) {
					edges.add(a + b + event);
					addLine(assembleEdgeLine(a, b, event));
				}

			}
		}
		return true;

	}

	public String getStateNameByContent(String[] stateContent) {
		for (Enumeration e = nodes.keys(); e.hasMoreElements();) {
			String key = (String) e.nextElement();
			String[] content = nodes.get(key);
			if (equal(content, stateContent))
				return key;
		}
		return null;
	}

	public boolean startFile(String fileName, String graphName) {

		try {
			outputFile = new BufferedWriter(new FileWriter(fileName));
			outputFile.write("digraph " + graphName + " { \r\n");
			outputFile.write("node [style=rounded] \r\n");
			return true;

		} catch (IOException e) {
			return false;
		}

	}

	public boolean closeFile() {

		try {

			outputFile.write("}\r\n");
			outputFile.close();
			return true;

		} catch (IOException e) {
			return false;
		}

	}

	public String assembleNodeLine(String label, String[] info) {
		String content = "";
		for (int i = 0; i < info.length; i++) {
			if (i > 0)
				content = content + "|" + info[i];
			else if (i == 0)
				content = info[i];
		}
		return label + "[shape=\"record\", label=\"{" + replaceChar(content)
				+ "}\"];";

	}

	public String assembleEdgeLine(String a, String b, String label) {
		return a + "->" + b + "[arrowhead = \"vee\",label=\"" + label + "\"];";
	}

	public String replaceChar(String content) {
		return content.replace(">", "\\>");
	}

	public boolean addLine(String line) {

		try {

			outputFile.write(line + "\r\n");
			return true;

		} catch (IOException e) {
			return false;
		}

	}

	public boolean isInNodes(String[] statecontent) {
		for (Enumeration e = nodes.keys(); e.hasMoreElements();) {
			String key = (String) e.nextElement();
			String[] content = nodes.get(key);
			if (equal(content, statecontent))
				return true;
		}
		return false;
	}

	public boolean equal(String[] object1, String[] object2) {
		if (object1.length != object2.length) {
			return false;
		}
		if ((object1.length == 0) && (object2.length == 0)) {
			return true;
		}
		boolean equal = false;
		int i = 0;
		while (i < object1.length) {
			if (!object1[i].trim().equals(object2[i].trim()))
				return false;
			i = i + 1;
			equal = true;
		}
		return equal;
	}

}
