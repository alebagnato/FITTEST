package eu.fittest.eventSequenceGenerator.data;

import java.io.PrintStream;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
*
* @author Alessandro
*
*/
public class Node {
	List<Edge> outEdges = new LinkedList<Edge>();
	String label;
	
	public Node(String label) {
		this.label = label;
	}
	
	public void addEdge(Edge e) {
		for (Edge f : outEdges) {
			if (e.getEvent().equals(f.getEvent()) && (e.getTarget() == f.getTarget())) {
				//se edge esiste
				return;
			}
		}
		
		outEdges.add(e);
	}
	
	public List<Edge> getSucc() {
		return outEdges;
	}
	
	public String getLabel() {
		return label;
	}
	
	void setLabel(String label) {
		this.label = label;
	}
	
	boolean accepts(String[] events) {
		if (events.length == 0) return true;
		String[] tail = new String[events.length - 1];
		for (int i = 1 ; i < events.length ; i++)
			tail[i-1] = events[i];
		String event = events[0];
		for (Edge e : outEdges) {
			if (e.getEvent().equals(event))
				return e.getTarget().accepts(tail);
		}
		return false;
	}

	void print() {
		print(System.out);
	}

	void print(PrintStream out)
	{
		for (Edge e : outEdges) {
			out.print(label + " -> [" +  e.getEvent() + "] " + e.getTarget().getLabel());
			if (e.getMarks().size() != 0) {
				out.print(" [");
				boolean first = true;
				for (String m : e.getMarks()) {
					if (!first) out.print(", ");			
					out.print(m);
					first = false;
				}
				out.print("]");
			}
			out.println(";");	
		}
	}
	
	public boolean equals(Node node2, Set<Node> visited1, Set<Node> visited2) {
		visited1.add(this);
		visited2.add(node2);
		if (outEdges.size() != node2.outEdges.size()) return false;
		for (Edge e1 : outEdges) {
			boolean found = false;
			boolean equals = true;
			for (Edge e2 : node2.outEdges) {
				if (e1.event.equals(e2.event)) {
					found = true;
					if (!visited1.contains(e1.target) && !visited2.contains(e2.target))
						equals = equals && e1.target.equals(e2.target, visited1, visited2);
				}
			}
			if (!found || !equals) return false;
		}
		return true;
	}
	
}
