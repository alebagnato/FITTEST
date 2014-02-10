package eu.fittest.fbk.efsm2ct.log2efsm.infer.manager;

import java.io.File;
import java.util.Vector;

import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.Fsm;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.Transition;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.modelInference.ModelInference;

/**
 * 
 * @author Alessandro Marchetto Manager of the FSM construction
 */
public class FSMmanager {

	private ModelInference modelInferrer = new ModelInference();

	private Fsm fsmAllInOne;

	public FSMmanager() {
		fsmAllInOne = new Fsm();
		resetOpsCounter();
	}

	public void resetOpsCounter() {
		fsmAllInOne.resetOpsCounter();
	}

	public void printFSM(Fsm f, String startString) {

		Vector<Vector<String[]>> s = f.convertFormat();
		Vector<String[]> sv;
		String tmp1 = "";
		String tmp2 = "";
		System.out.println(startString);
		for (int i = 0; i < s.size(); i++) {
			sv = s.get(i);
			tmp1 = "";
			for (int j = 0; j < sv.get(0).length; j++) {
				tmp1 = tmp1 + " " + sv.get(0)[j];
			}
			tmp2 = "";
			for (int j = 0; j < sv.get(2).length; j++) {
				tmp2 = tmp2 + " " + sv.get(2)[j];
			}

			System.out.println(" " + tmp1 + "-" + sv.get(1)[0] + "-" + tmp2 + "");
		}
	}

	public Fsm generateFSM(File[] filesSequence, String logFilenamePrefix, int execIndex) {

		fsmAllInOne = modelInferrer.statesInference(filesSequence, logFilenamePrefix, execIndex);

		return fsmAllInOne;

	}

	public void removeEdges(int currentExecIndex) {
		Vector<Long> remove = fsmAllInOne.getKeyOfEdgeAndExecIndex_ByValue(currentExecIndex);
		long idTransitionToBeRemoved;
		boolean removed = false;
		for (int i = 0; i < remove.size(); i++) {
			idTransitionToBeRemoved = remove.get(i);

			removed = fsmAllInOne.removeTransition(idTransitionToBeRemoved);
			if (removed)
				fsmAllInOne.remops = fsmAllInOne.remops + 1;

		}
		fsmAllInOne.cleaningFsm();

	}

	public Vector<Vector<String[]>> getConvertedEdges() {
		Vector<Vector<String[]>> vvs = fsmAllInOne.convertFormat();
		return vvs;
	}

	public int addEdgeToFSM(Fsm fsmToAdd, int execIndex, int countEventsinLogs) {
		boolean added = false;

		for (Transition tran : fsmToAdd.transitions.getTransitions()) {
			execIndex++;
			added = fsmAllInOne.addItemToStates(fsmToAdd.states.getStateById(tran.getIdStateSource()), tran, fsmToAdd.states.getStateById(tran.getIdStateTarget()), execIndex);
			if (added) {
				fsmAllInOne.addops = fsmAllInOne.addops + 1;
			}
			// fsmAllInOne.checks=fsmAllInOne.checks+1; //reale per la macchina
			// main e basta

		}
		fsmAllInOne.checks = fsmAllInOne.checks + countEventsinLogs; // tiene
																		// conto
																		// di
																		// tutti
																		// (siam
																		// macchina
																		// main
																		// che
																		// quella
																		// corrente
																		// relativa
																		// al
																		// log
																		// corrente)
		fsmAllInOne.eventsinlogs = fsmAllInOne.eventsinlogs + countEventsinLogs;
		return execIndex;

	}

	public Vector<Transition> getOngoingTransitions(long stateSourceId) {
		Vector<Transition> tout = new Vector<Transition>();
		for (int i = 0; i < fsmAllInOne.transitions.getTransitions().size(); i++) {
			if (fsmAllInOne.transitions.getTransitions().get(i).getIdStateSource() == stateSourceId) {
				tout.add(fsmAllInOne.transitions.getTransitions().get(i));
			}
		}
		return tout;
	}

}
