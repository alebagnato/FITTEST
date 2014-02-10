package eu.fittest.modelInference.fsmInference.fsm;

import java.util.Vector;

import eu.fittest.modelInference.fsmInference.utility.Utility;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Transitions {

	Vector<Transition> transitions;
	Utility utils = new Utility();

	public Transitions() {
		transitions = new Vector<Transition>();
	}

	public Vector<Transition> getTransitions() {
		return transitions;
	}

	public Transition getTransitionById(long id) {
		Transition t;
		for (int i = 0; i < size(); i++) {
			t = transitions.get(i);
			if (t.getidTransition() == id)
				return t;
		}

		return null;
	}

	public Transition getTransitionByElements(long sourceStateId,
			long targetStateId, long eventId) {
		Transition t;

		for (int i = 0; i < size(); i++) {
			t = transitions.get(i);

			if (t.getidTransition() == eventId)
				return t;
		}

		return null;
	}

	public Vector<Transition> getTransitionsBySourceId_and_TargetId(
			long sourceStateId, long targetStateId) {
		Transition t;
		Vector<Transition> ts = new Vector<Transition>();

		for (int i = 0; i < size(); i++) {
			t = transitions.get(i);

			if ((t.idStateSource == sourceStateId)
					&& (t.idStateTarget == targetStateId)) {
				ts.add(t);
			}

		}

		return ts;
	}

	public Vector<Transition> getTransitionsBy_SourceId(long sourceStateId) {
		Transition t;
		Vector<Transition> ts = new Vector<Transition>();

		for (int i = 0; i < size(); i++) {
			t = transitions.get(i);
			if (t.idStateSource == sourceStateId) {
				ts.add(t);
			}

		}

		return ts;
	}

	public Vector<Transition> getTransitionsBy_TargetId(long targetStateId) {
		Transition t;
		Vector<Transition> ts = new Vector<Transition>();

		for (int i = 0; i < size(); i++) {
			t = transitions.get(i);
			if (t.idStateTarget == targetStateId) {
				ts.add(t);
			}

		}

		return ts;
	}

	public boolean addTransition(Transition transition) {
		Transition t = transition.clone();

		Transition ret = exists(t);

		if (ret == null) {
			transitions.add(t);
			return true;
		} else {
			ret.setExcFreqTransition(ret.execFreq + 1);
		}

		return false;
	}

	public int size() {
		return transitions.size();
	}

	public Transition exists(Transition t) {
		Transition tmp;
		for (int i = 0; i < size(); i++) {
			tmp = transitions.get(i);
			if ((tmp.idStateSource == t.getIdStateSource())
					&& (tmp.idStateTarget == t.getIdStateTarget())) {
				if (utils.equalContent(tmp.getTransitionContent(),
						t.getTransitionContent()))
					return tmp;
			}
		}
		return null;
	}

	public long exists(String[] transitionContent) {
		Transition t;
		String[] tC;

		for (int i = 0; i < size(); i++) {
			t = transitions.get(i);
			tC = t.getTransitionContent();
			if (utils.equalContent(transitionContent, tC))
				return t.getidTransition();

		}
		return -1;
	}

	public boolean removeTransitionById(long id) {
		Transition t;

		int toberemoved = -1;

		for (int i = 0; i < transitions.size(); i++) {
			t = transitions.get(i);
			if (t.getidTransition() == id) {
				toberemoved = i;
				break;
			}
		}
		if (toberemoved > -1) {
			transitions.remove(toberemoved);
			return true;
		}

		return false;

	}

	public long getHigherTransitionID() {
		Transition t;
		long higher = -1;
		for (int i = 0; i < size(); i++) {
			t = transitions.get(i);
			if (t.getidTransition() > higher)
				higher = t.getidTransition();
		}

		return higher;
	}

	public int getHigherTransitionExecIndex() {
		Transition t;
		int higher = 0;
		for (int i = 0; i < size(); i++) {
			t = transitions.get(i);
			if (t.getTransitionExecIndex() > higher)
				higher = t.getTransitionExecIndex();
		}

		return higher;
	}

	public int getLowerTransitionExecIndex() {
		Transition t;
		int lower = 0;
		for (int i = 0; i < size(); i++) {
			t = transitions.get(i);
			if ((t.getTransitionExecIndex() < lower)
					&& (t.getTransitionExecIndex() >= 0))
				lower = t.getTransitionExecIndex();
		}

		return lower;
	}

	@Override
	public Transitions clone() {
		Transitions clone = new Transitions();
		for (int i = 0; i < this.transitions.size(); i++) {
			clone.addTransition(this.transitions.get(i));
		}
		return clone;
	}
}
