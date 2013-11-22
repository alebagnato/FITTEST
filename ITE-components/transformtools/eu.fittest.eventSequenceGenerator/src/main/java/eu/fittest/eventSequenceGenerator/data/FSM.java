/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.eventSequenceGenerator.data;

import java.io.BufferedReader;

import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
*
* @author Alessandro
*
*/
public class FSM {
	Node startNode = null;
	Map<String, Node> label2Node = new HashMap<String, Node>();
	
	public FSM(String fileName) {
		read(fileName);
	}
	
	public FSM() {}
	
	int size() {
		return label2Node.size();
	}
	
	public boolean equals(Object obj) {
		FSM fsm = (FSM)obj;
		Set<Node> visited1 = new HashSet<Node>();
		Set<Node> visited2 = new HashSet<Node>();
		return this.startNode.equals(fsm.getStartNode(), visited1, visited2);
	}
	
	void addGenStrings(Vector<List<String>> genStrings, int maxLength, List<String> curPath, Node n) {
		if (maxLength == 0 || n.getSucc().size() == 0) {
			genStrings.add(curPath);
			return;
		}
		for (Edge e: n.getSucc()) {
			Node m = e.getTarget();
			String t = e.getEvent();
			List<String> newPath = new LinkedList<String>();
			newPath.addAll(curPath);
			newPath.add(t);
			addGenStrings(genStrings, maxLength-1, newPath, m);
		}		
	}
	
	Vector<List<String>> generateStrings(int maxLength) {
		Vector<List<String>> genStrings = new Vector<List<String>>();
		List<String> curPath = new LinkedList<String>();
		addGenStrings(genStrings, maxLength, curPath, startNode);
		return genStrings;
	}
	
	public boolean accepts(String[] events) {
		return startNode.accepts(events);
	}
	
	boolean accepts(List<String> events) {
		Object[] objs = events.toArray();
		String[] evts = new String[objs.length];
		for (int i = 0 ; i < objs.length ; i++)
			evts[i] = (String)objs[i];
		return startNode.accepts(evts);
	}

	List<String> serialPath() {
		List<String> serialPath = new LinkedList<String>();
		Node curNode = startNode;
		while (curNode.outEdges.size() > 0) {
			serialPath.add(curNode.getSucc().get(0).getEvent());
			curNode = curNode.getSucc().get(0).getTarget();
		}
		return serialPath;
	}
	
	public void regenerateLabels() {
		Map<String, Node> newLabel2Node = new HashMap<String, Node>();
		Node newStartNode = null;
		int nodeId = 1;
		for (String s : label2Node.keySet()) {
			String newLabel = "S" + (nodeId++);
			Node n = label2Node.get(s);
			n.setLabel(newLabel);
			newLabel2Node.put(newLabel, n);
			if (n == startNode)
				newStartNode = n;
		}
		startNode = newStartNode;
		label2Node = newLabel2Node;
	}
	
	void addAllMark(String m) {
		for (Node n : getNodes())
			for (Edge e : n.getSucc())
				e.addMark(m);
	}

	void removeAllMarks() {
		for (Node n : getNodes())
			for (Edge e : n.getSucc())
				e.getMarks().clear();
	}

	void makeMarksUnique() {
		Map<String, Integer> c = new HashMap<String, Integer>();
		for (Node n : getNodes())
			for (Edge e : n.getSucc()) {
				Set<String> newMarks = new HashSet<String>();
				for (String s : e.getMarks()) {
					if (!c.containsKey(s)) c.put(s, 0);
					c.put(s, c.get(s) + 1);
					newMarks.add(s + c.get(s));
				}
				e.getMarks().clear();
				e.getMarks().addAll(newMarks);
			}
	}
	
	/**
	 * States get prefixed in the clone. E.g., S1, S2, ... --> M1_S1, M1_S2, M1_S3
	 * @param prefix
	 * @return
	 */
	public FSM clone(String prefix) {
		FSM fsm = new FSM();
		Map<String, Node> label2Node = new HashMap<String, Node>();
		for (Node n : this.getNodes()) {
			if (label2Node.get(n.getLabel()) == null) {
				String s = prefix + n.getLabel();
				fsm.addNode(s);
				label2Node.put(n.getLabel(), fsm.getNode(s));
			}
			if (this.getStartNode() == n) {
				fsm.setStartNode(label2Node.get(n.getLabel()));
			}
			Node node = label2Node.get(n.getLabel());
			for (Edge e : n.getSucc()) {
				if (label2Node.get(e.getTarget().getLabel()) == null) {
					String s = prefix + e.getTarget().getLabel();
					fsm.addNode(s);
					label2Node.put(e.getTarget().getLabel(), fsm.getNode(s));
				}
				node.addEdge(new Edge(label2Node.get(e.getTarget().getLabel()), e.getEvent(), e.getMarks()));
			}
		}
		return fsm;
	}
	
	public Node getStartNode() {
		return startNode;
	}
	
	void setStartNode(Node startNode) {
		this.startNode = startNode;
	}
	
	public Collection<Node> getNodes() {
		return label2Node.values();
	}
	
	Node getNode(String label) {
		return label2Node.get(label);
	}
	
	void addNode(String label, Node node) {
		label2Node.put(label, node);
	}
	
	void addNode(String label) {
		if (getNode(label) == null)
			label2Node.put(label, new Node(label));
	}

	void removeNode(String label) {
		if (getNode(label) != null)
			label2Node.remove(label);
	}
	
	private void read(String fileName) {
		try {
			BufferedReader in = new BufferedReader(new FileReader(fileName));
			String s;
			//Pattern p = Pattern.compile("\\s*(\\w+)\\s*->\\s*\\[\\s*([\\w|\\-|\\s]+)\\s*\\]\\s*(\\w+)\\s*;\\s*");
			Pattern p = Pattern.compile("\\s*(\\w+)\\s*->\\s*\\[\\s*(.*)\\s*\\]\\s*(\\w+)\\s*;\\s*");
			while ((s = in.readLine()) != null) {
				Matcher m = p.matcher(s);
				if (m.matches()) {
					addNode(m.group(1));
					addNode(m.group(3));
					if (startNode == null) startNode = getNode(m.group(1));
					getNode(m.group(1)).addEdge(new Edge(getNode(m.group(3)), m.group(2)));
				} else {
					System.err.println("WARNING: skipping " + s);
				}
			}
		} catch (IOException e) {
			System.err.println("IO error while reading: " + fileName);
			System.exit(1);
		}
	}
	
	public void print() {
		print(System.out);
	}
	
	public void print(PrintStream out)
	{
		Set<String> printed = new HashSet<String>();
		print(startNode, printed, out);
	}
	
	void print(Node n, Set<String> printed, PrintStream out) {
		if (printed.contains(n.getLabel())) return;
		printed.add(n.getLabel());
		n.print(out);
		for (Edge e : n.getSucc())
			print(e.getTarget(), printed, out);
	}

	
	//
	public Node getNodeByLabel(String label) {
		Node n=null;
		n=getNode(label);
		return n;
	}

	
}
