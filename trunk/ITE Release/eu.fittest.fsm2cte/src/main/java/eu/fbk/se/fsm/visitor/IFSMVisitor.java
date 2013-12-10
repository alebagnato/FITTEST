package eu.fbk.se.fsm.visitor;

import java.util.Vector;

import eu.fbk.se.fsm.FSM;

public interface IFSMVisitor {
	public void visit(FSM fsm);

	public Vector<Path> getPaths();
}
