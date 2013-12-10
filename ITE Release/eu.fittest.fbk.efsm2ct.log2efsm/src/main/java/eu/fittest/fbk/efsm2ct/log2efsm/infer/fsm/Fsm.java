package eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm;

import java.util.Vector;

import eu.fittest.fbk.efsm2ct.log2efsm.infer.config.Config_Launcher;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.utility.Utility;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Fsm {

	private Utility utils = new Utility();
	private Config_Launcher config = Config_Launcher.getInstace();

	public States states;
	public Transitions transitions;

	public int checks = 0;
	public int addops = 0;
	public int remops = 0;
	public int eventsinlogs = 0;

	public Fsm() {
		this.states = new States();
		this.transitions = new Transitions();
	}

	public void resetOpsCounter() {
		checks = 0;
		addops = 0;
		remops = 0;
		eventsinlogs = 0;
	}

	public Vector<Long> getKeyOfEdgeAndExecIndex_ByValue(int value) {
		Vector<Long> tobedeleted = new Vector<Long>();

		for (int i = 0; i < transitions.size(); i++) {
			if (transitions.getTransitions().get(i).getTransitionExecIndex() == value) {
				tobedeleted.add(transitions.getTransitions().get(i).getidTransition());
			}
		}

		return tobedeleted;
	}

	public boolean addItemToStates(State stateBefore, Transition event, State stateAfter, int execIndex) {

		boolean added = addItemToStates(stateBefore.getStateContent(), event.getTransitionContent()[0], stateAfter.getStateContent(), execIndex);

		return added;

	}

	public boolean addItemToStates(String[] stateBefore, String event, String[] stateAfter, int execIndex) {
		String[] events = null;

		boolean added = false;

		long tid = contains_TransitionId(stateBefore, event, stateAfter);

		long before = -1;
		long after = -1;

		if (tid == -1) {

			before = states.exists(stateBefore);
			if (before == -1) {
				State state1 = new State(utils.copy(stateBefore));
				before = state1.getId();
				states.addStates(state1);
			}

			after = states.exists(stateAfter);
			if (after == -1) {
				State state2 = new State(utils.copy(stateAfter));
				after = state2.getId();
				states.addStates(state2);
			}

			events = new String[1];
			events[0] = event;
			Transition transition = new Transition(before, after);
			transition.setTransitionContent(utils.copy(events));
			transition.setTransitionExecIndex(execIndex);
			added = transitions.addTransition(transition);

		} else {
			if (transitions.getTransitionById(tid) != null)
				transitions.getTransitionById(tid).setTransitionExecIndex(execIndex);
			added = false;
		}

		return added;

	}

	public Vector<Vector<String[]>> getEdgesInConvertedFormat() {
		return convertFormat();
	}

	public Vector<String[]> getConvertedEdgesByIndex(int index) {
		Vector<Vector<String[]>> v = getEdgesInConvertedFormat();
		return v.get(index);
	}

	public int getNumberOfConvertedEdges() {
		Vector<Vector<String[]>> v = getEdgesInConvertedFormat();
		return v.size();
	}

	public long contains_TransitionId(String[] stateBefore, String event, String[] stateAfter) {

		String[] events = new String[1];
		events[0] = event;

		Transition t;
		long idT = -1;
		long idSource = -1;
		long idTarget = -1;

		idT = transitions.exists(events);
		if (idT > -1) {
			t = transitions.getTransitionById(idT);

			idSource = t.getIdStateSource();
			idTarget = t.getIdStateTarget();

			if ((idSource > -1) && (idTarget > -1)) {
				if (utils.equalContent(states.getStateById(idSource).stateContent, stateBefore)) {
					if (utils.equalContent(states.getStateById(idTarget).stateContent, stateAfter)) {
						return idT;
					}
				}
			}

		}
		return -1;

	}

	public void cleaningFsm() {
		long idStateTocheck;
		boolean isused = false;
		for (int i = 0; i < states.size(); i++) {
			idStateTocheck = states.getStates().get(i).getId();
			isused = false;
			int j = 0;
			while ((j < transitions.size()) && (isused == false)) {
				if ((transitions.getTransitions().get(j).idStateSource == idStateTocheck) || (transitions.getTransitions().get(j).idStateTarget == idStateTocheck)) {
					isused = true;
				}
				j++;
			}
			if (isused == false) {
				removeState(idStateTocheck);
			}
		}

	}

	public boolean removeTransition(long idTransitionToBeRemoved) {
		return transitions.removeTransitionById(idTransitionToBeRemoved);
	}

	public void removeState(long idStateToBeRemoved) {
		states.removeStateById(idStateToBeRemoved);
	}

	public Vector<Vector<String[]>> convertFormat() {
		Vector<Vector<String[]>> trans = new Vector<Vector<String[]>>();
		Vector<String[]> tran;

		String[] contentBefore;
		String[] contentAfter;

		State s;

		for (int i = 0; i < transitions.size(); i++) {
			tran = new Vector<String[]>();

			s = states.getStateById(transitions.getTransitions().get(i).idStateSource);
			if (s != null) {
				contentBefore = utils.copy(s.getStateContent());

				s = states.getStateById(transitions.getTransitions().get(i).idStateTarget);
				if (s != null) {
					contentAfter = utils.copy(s.getStateContent());

					tran.add(contentBefore);
					tran.add(utils.copy(transitions.getTransitions().get(i).getTransitionContent()));
					tran.add(contentAfter);
					trans.add(tran);

				}

			}

		}

		return trans;
	}

	@Override
	public Fsm clone() {
		Fsm f = new Fsm();

		Transition t;
		State source;
		State target;
		int execIndex;

		for (int i = 0; i < this.transitions.getTransitions().size(); i++) {
			t = this.transitions.getTransitions().get(i);
			source = this.states.getStateById(t.idStateSource);
			target = this.states.getStateById(t.idStateTarget);
			execIndex = t.getTransitionExecIndex();

			f.addItemToStates(source.getStateContent(), t.getTransitionContent()[0], target.getStateContent(), execIndex);
		}

		return f;
	}

	public double eventDensity(String event) {

		double count = 0.0;

		for (int i = 0; i < transitions.size(); i++) {
			if (transitions.getTransitions().get(i).getTransitionContent()[0].equalsIgnoreCase(event)) {
				count++;
			}
		}

		if (count > 0.0) {
			return count / (new Double(transitions.size()).doubleValue());
		}

		return 0;
	}

	public Vector<Transition> getOngoingTransitions(long stateSourceId) {
		Vector<Transition> tout = new Vector<Transition>();
		for (int i = 0; i < transitions.getTransitions().size(); i++) {
			if (transitions.getTransitions().get(i).getIdStateSource() == stateSourceId) {
				tout.add(transitions.getTransitions().get(i));
			}
		}
		return tout;
	}

}
