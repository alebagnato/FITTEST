package eu.fbk.se.fsm;

import java.io.File;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class ComputeModelFromTraces {

	private final int MAX_FSM_SIZE = 5000; // by urueda

	private final TraceFileReader traceFileReader;
	int nodeId = 1;

	public ComputeModelFromTraces(TraceFileReader traceFileReader) {
		this.traceFileReader = traceFileReader;
	}

	FSM readTrace() throws IOException {
		FSM fsm = new FSM();
		String s;
		String curNode = "S" + nodeId++;
		fsm.addNode(curNode);
		fsm.setStartNode(fsm.getNode(curNode));
		String prevNode = curNode;
		// Pattern p = Pattern.compile("\\s*(\\w+)\\s*.*"); //org
		while ((s = traceFileReader.nextEvent()) != null) {
			curNode = "S" + nodeId++;
			fsm.addNode(curNode);
			fsm.getNode(prevNode).addEdge(new Edge(fsm.getNode(curNode), s));
			prevNode = curNode;
		}
		return fsm;
	}

	FSM readTrace(String fileName) {
		FSM fsm = new FSM();
		try {
			traceFileReader.open(new FileReader(fileName));
			fsm = readTrace();
			traceFileReader.close();
		} catch (IOException e) {
			System.err.println("IO error while reading: " + fileName);
			e.printStackTrace();
		}
		return fsm;
	}

	public FSM readTraces(String traceFolderPath, String traceFile_Extension)
			throws MaxFSMSizeExceededException {

		String[] fileNames = getFileNames(traceFolderPath, traceFile_Extension);

		return readTraces(fileNames);
	}

	FSM readTraces(String fileNames[]) throws MaxFSMSizeExceededException {
		FSMAlgo algo = new FSMAlgo();
		FSM fsm[] = new FSM[fileNames.length];
		Node start = new Node("U1");
		FSM u = new FSM();
		u.addNode("U1", start);
		u.setStartNode(start);
		for (int i = 0; i < fileNames.length; i++) {
			fsm[i] = readTrace(fileNames[i]);
			for (Node n : fsm[i].getNodes())
				if (fsm[i].getStartNode() != n)
					u.addNode(n.getLabel(), n);
			for (Edge e : fsm[i].getStartNode().getSucc())
				start.addEdge(new Edge(
						fsm[i].getNode(e.getTarget().getLabel()), e.getEvent(),
						e.getMarks()));
		}
		u = algo.makeDeterministic(u, MAX_FSM_SIZE); //1000); // by urueda
		u = algo.kTail(u, 2);
		u = algo.makeDeterministic(u, MAX_FSM_SIZE); //1000); // by urueda
		return u;
	}

	static public void main(String args[]) {
		if (args.length < 1) {
			System.err
					.println("Usage java eu.fbk.se.fsm.ComputeModelFromTraces trace1.trc trace2.trc ...");
			System.exit(1);
		}
		ComputeModelFromTraces cm = new ComputeModelFromTraces(new TrcReader());
		FSM fsm;
		try {
			fsm = cm.readTraces(args);
			fsm = cm.makeReadable(fsm);
			fsm.print();
		} catch (MaxFSMSizeExceededException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Rename the nodes to make them readable
	 * 
	 * @author cunduy
	 * 
	 * @param input
	 * @return
	 */
	public FSM makeReadable(FSM input) {
		FSM ret = input.clone("");
		Map<String, String> nameMap = new HashMap<String, String>(); // need to
																		// deal
																		// with
																		// nodes
																		// having
																		// same
																		// name
		Node firstNode = ret.getStartNode();
		int i = 1;
		nameMap.put(firstNode.getLabel(), "START");
		for (Node node : ret.getNodes()) {
			String oldName = node.getLabel();
			String newName = nameMap.get(oldName);
			if (newName == null) {
				newName = "S" + String.valueOf(i++);
				nameMap.put(oldName, newName);
			}
			node.setLabel(newName);
		}

		return ret;
	}

	public static String[] getFileNames(String folderName, final String ext) {
		ArrayList<String> traces = new ArrayList<String>();
		File dir = new File(folderName);
		FilenameFilter traceFilter = new FilenameFilter() {
			@Override
			public boolean accept(File arg0, String arg1) {
				// return arg1.toLowerCase().endsWith(ext) &&
				// arg1.toLowerCase().startsWith("log_");
				return arg1.toLowerCase().endsWith(ext);
			}
		};

		String[] traceFiles = dir.list(traceFilter);

		for (String trace : traceFiles) {
			traces.add(folderName + System.getProperty("file.separator")
					+ trace);
		}

		String[] traceFileNames = new String[traces.size()];
		traces.toArray(traceFileNames);
		return traceFileNames;
	}
}
