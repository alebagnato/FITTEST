package eu.fbk.se.fsm;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.Vector;


public class FSMAlgo {
	
	final String prefix = "S"; // state id = prefix + counter; e.g., S1, S2, etc.
	final Random ran = new Random();
	
	public FSM makeDeterministic(FSM fsm, int maxSize) throws MaxFSMSizeExceededException {
		FSM d = new FSM();
		//Node start = new Node("D1");
		Node start = new Node(fsm.getStartNode().getLabel());
		//d.addNode("D1", start);
		d.addNode(fsm.getStartNode().getLabel(), start);
		d.setStartNode(start);
		Set<Node> startNodes = new HashSet<Node>();
		startNodes.add(fsm.getStartNode());
		d = makeDeterministic(d, d.getStartNode(), startNodes, maxSize);
		return d;
	}
	
	private FSM makeDeterministic(FSM d, Node start, Set<Node> startNodes, int maxSize) throws MaxFSMSizeExceededException {
		System.out.println("FSM size = " + d.size()); // by urueda
		if (d.size() > maxSize) {
			System.out.println("FSM exceeded the size: " + (new Integer(maxSize)).toString()); // by urueda
			throw new MaxFSMSizeExceededException();
     	}	
		Set<String> found = new HashSet<String>();
		for (Node n : startNodes) {
			for (Edge e : n.getSucc()) {
				String event = e.getEvent();
				if (found.contains(event)) continue;
				found.add(event);
				Set<Node> targetNodes = new HashSet<Node>();
				Set<String> marks = new HashSet<String>();
				for (Node m : startNodes)
					for (Edge f : m.getSucc())
						if (f.getEvent().equals(event)) {
							targetNodes.add(f.getTarget());
							marks.addAll(f.getMarks());
						}
				List<String> ids = new LinkedList<String>();
				for (Node t : targetNodes)
					ids.add(t.getLabel());
				Collections.sort(ids);
				String id = ids.get(0);
				for (int i = 1 ; i < ids.size() ; i++)
					id = id + "_" + ids.get(i);
				boolean targetExplored =  (d.getNode(id) != null);
				d.addNode(id);
				start.addEdge(new Edge(d.getNode(id), event, marks));
				if (!targetExplored)
				{
					d = makeDeterministic(d, d.getNode(id), targetNodes, maxSize);
				}
			}
		}
		return d;
	}

	/**
	 * Non deterministic intersection.
	 * Input: two deterministic FSMs.
	 * @param fsm1
	 * @param fsm2
	 * @return
	 */
	public FSM intersection(FSM fsm1, FSM fsm2) {
		FSM i = new FSM();
		Node start = new Node("I1");
		i.addNode("I1", start);
		i.setStartNode(start);
		intersection(i, start, fsm1.getStartNode(), fsm2.getStartNode());
		return i;
	}
	
	void intersection(FSM i, Node n, Node n1, Node n2) {
		for (Edge e1 : n1.getSucc())
			for (Edge e2 : n2.getSucc())
				if (e1.getEvent().equals(e2.getEvent())) {
					String id = e1.getTarget().getLabel() + "_" + e2.getTarget().getLabel();
					Set<String> marks = new HashSet<String>();
					marks.addAll(e1.getMarks());
					marks.addAll(e2.getMarks());
					boolean targetExplored =  (i.getNode(id) != null);
					if (!targetExplored)
					{
						i.addNode(id);
						n.addEdge(new Edge(i.getNode(id), e1.getEvent(), marks));
						intersection(i, i.getNode(id), e1.getTarget(), e2.getTarget());
					}
				}
	}
	
	/** 
	 * Non deterministic union.
	 * @param fsm1
	 * @param fsm2
	 * @return
	 */
	public FSM union(FSM fsm1, FSM fsm2) {
		FSM u = new FSM();
		Node start = new Node("U1");
		u.addNode("U1", start);
		u.setStartNode(start);
		
		fsm1 = fsm1.clone("M1_");
		fsm2 = fsm2.clone("M2_");
		Node start1 = fsm1.getStartNode();
		Node start2 = fsm2.getStartNode();
		
		for (Node n : fsm1.getNodes())
		{
			if (n != start1)
			{
				u.addNode(n.getLabel(), n);
				//check if any of n's out edges are to start node
				for (Edge e : n.getSucc()) {
					if (e.getTarget() == start1)
						e.setTarget(start);
				}
			}
		}
		for (Node n : fsm2.getNodes())
		{
			if (n != start2)
			{
				u.addNode(n.getLabel(), n);
				//check if any of n's out edges are to start node
				for (Edge e : n.getSucc()) {
					if (e.getTarget() == start2)
						e.setTarget(start);
				}
			}
		}		
		for (Edge e : start1.getSucc()) {
			if (e.getTarget() == start1)
				start.addEdge(new Edge(start, e.getEvent(), e.getMarks()));
			else
				start.addEdge(new Edge(u.getNode(e.getTarget().getLabel()), e.getEvent(), e.getMarks()));
		}
		for (Edge e : start2.getSucc()) {
			if (e.getTarget() == start2)
				start.addEdge(new Edge(start, e.getEvent(), e.getMarks()));
			else
				start.addEdge(new Edge(u.getNode(e.getTarget().getLabel()), e.getEvent(), e.getMarks()));
		}
		
		return u;
	}

	Set<String> getTail(Set<String> t, Node n, int k) {
		if (k == 0) return t; 
		Set<String> ttt = new HashSet<String>();
		for (Edge e : n.getSucc()) {
			Set<String> tt = new HashSet<String>();
			for (String s : t) {
				if (s.length() != 0)
					tt.add(s + ":" + e.getEvent());
				else 
					tt.add(e.getEvent());
			}
			ttt.addAll(getTail(tt, e.getTarget(), k-1));
		}
		return ttt;
	}
	
	public boolean checkTail(Node n1, Node n2, int k) {
		Set<String> t1 = new HashSet<String>();
		t1.add("");		
		t1 = getTail(t1, n1, k);
		Set<String> t2 = new HashSet<String>();
		t2.add("");		
		t2 = getTail(t2, n2, k);
		if (t1.size() == 0 || t2.size() == 0) return false;
		return t1.containsAll(t2) || t2.containsAll(t1);
	}
	
	public void merge(FSM fsm, Node n1, Node n2) {
		String id = n1.getLabel() + "_" + n2.getLabel();
		fsm.addNode(id);
		Node n = fsm.getNode(id);
		if (fsm.getStartNode() == n1 || fsm.getStartNode() == n2)
			fsm.setStartNode(n);
		for (Node m : fsm.getNodes())
			if (m != n)
				if (m == n1 || m == n2) {
					for (Edge e : m.getSucc())
						if (e.getTarget() == n1 || e.getTarget() == n2)
							n.addEdge(new Edge(n, e.getEvent(), e.getMarks()));
						else
							n.addEdge(new Edge(e.getTarget(), e.getEvent(), e.getMarks()));				
				} else {
					for (Edge e : m.getSucc())
						if (e.getTarget() == n1 || e.getTarget() == n2)
							e.setTarget(n);			
				}
		fsm.removeNode(n1.getLabel());
		fsm.removeNode(n2.getLabel());
	}
	
	FSM kTail(FSM fsm, int k) {
		FSM kFsm = fsm.clone("");
		boolean done = false;
		while (!done) {
			done = true;
			outer: for (Node n1 : kFsm.getNodes())
				for (Node n2 : kFsm.getNodes()) 
					if (n1 != n2 && checkTail(n1, n2, k)) {
						merge(kFsm, n1, n2);
						done = false;
						break outer;
					}
		}
		return kFsm;
	}

	public FSM randomMerge(FSM fsm) {
		Object[] nodes = fsm.getNodes().toArray();
		if (nodes.length <= 2) return fsm;
		int i = ran.nextInt(nodes.length);
		int j = ran.nextInt(nodes.length);
		while (i == j) j = ran.nextInt(nodes.length);
		merge(fsm, (Node)nodes[i], (Node)nodes[j]);
		return fsm;
	}
	
	public FSM randomKTail(FSM fsm, int k) {
		FSM kFsm = fsm.clone("");
		Object[] nodes = kFsm.getNodes().toArray();
		if (nodes.length <= 2) return fsm;
		Vector<Integer> index = new Vector<Integer>();
		for (int i = 0 ; i < nodes.length ; i++)
			index.add(i);
		int randIndex[] = new int[nodes.length];
		for (int i = 0 ; i < nodes.length ; i++) {
			int j = ran.nextInt(index.size());
			randIndex[i] = index.elementAt(j);
			index.removeElementAt(j);
		}
		done: for (int i = 0 ; i < randIndex.length ; i++) {
			for (int j = 0 ; j < randIndex.length ; j++) {
				if (randIndex[i] != randIndex[j]) {
					Node n1 = (Node)nodes[randIndex[i]];
					Node n2 = (Node)nodes[randIndex[j]];
					if (checkTail(n1, n2, k)) {
						merge(kFsm, n1, n2);
						break done;
					}
				}				
			}
		}
		return kFsm;
	}

	static public void main(String args[]) {
		if (args.length < 1) {
			System.err.println("Usage java eu.fbk.se.fsm.FSMAlgo file1.fsm");
			System.exit(1);
		}
		FSM u = new FSM(args[0]);
		u.print();
		
//		FSM fsm2 = new FSM(args[1]);
//		FSMAlgo algo = new FSMAlgo();
//		FSM u = algo.union(fsm1, fsm2);
//		try {
			//u = algo.makeDeterministic(u, 1000);
//			u = algo.kTail(u, 3);
			//u = algo.makeDeterministic(u, 1000);
			//u.regenerateLabels();
//			u.print();
//		} catch (MaxFSMSizeExceededException e) {
//			e.printStackTrace();
//		}
	}

	
}
