package eu.fittest.modelInference.fsmInference.manager;

import java.io.File;
import java.util.Vector;

import eu.fittest.modelInference.fsmInference.fsm.Fsm;
import eu.fittest.modelInference.fsmInference.fsm.StatesSet;
import eu.fittest.modelInference.fsmInference.fsm.Transition;
import eu.fittest.modelInference.fsmInference.modelInference.ConvertFSMXpr;
import eu.fittest.modelInference.fsmInference.modelInference.FSMtoDOT;
import eu.fittest.modelInference.fsmInference.modelInference.ModelInference;
import eu.fittest.modelInference.fsmInference.utility.Utility;

/**
 * 
 * @author Alessandro Marchetto Manager of the FSM construction
 */
public class FSMmanager {

	ModelInference p = new ModelInference();
	LogSequences logseqs = LogSequences.getInstace();

	FSMtoDOT fsm2dot = null;
	ConvertFSMXpr convertFsmXpr = null;
	Utility utils = new Utility();
	public Fsm fsmAllInOne;

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

			System.out.println(" " + tmp1 + "-" + sv.get(1)[0] + "-" + tmp2
					+ "");
		}
	}

	public Fsm generateFSM(File[] filesSequence, boolean reset, int execIndex) {

		if (reset)
			fsmAllInOne = new Fsm();

		fsmAllInOne = p.statesInference(filesSequence, execIndex);
		// StatesSet states=p.getStates();

		// fsmAllInOne=states.getAllStatesSet().get(0);
		// storeStates(states, execIndex);

		return fsmAllInOne;

	}

	public boolean storeStates(StatesSet states, int execIndex) {
		Vector<Fsm> vStates = states.getAllStatesSet();

		for (Fsm fsm : vStates) {
			addEdgeToFSM(fsm, execIndex, states.getCountEventsInLogs());
		}
		return true;

	}

	public void removeEdges(int currentExecIndex) {
		Vector<Long> remove = fsmAllInOne
				.getKeyOfEdgeAndExecIndex_ByValue(currentExecIndex);
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
			added = fsmAllInOne.addItemToStates(
					fsmToAdd.states.getStateById(tran.getIdStateSource()),
					tran,
					fsmToAdd.states.getStateById(tran.getIdStateTarget()),
					execIndex);
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

	public void FSM2FsmXpr(String fileName, String dirName, String ext) {
		convertFsmXpr = new ConvertFSMXpr(fsmAllInOne.transitions);
		// convertFsmXpr.convertTransitions(fileName,dirName,ext); //original
		// states in terms of id
		convertFsmXpr.convertTransitions_readableLabels(fileName, dirName, ext); // more
																					// readable
																					// states
	}

	public void FSM2DOT(Fsm fsm, String fsm2dotFileName) {
		fsm2dot = new FSMtoDOT();
		fsm2dot.apply(fsm, fsm2dotFileName, "FSM");
	}

	public void FSM2DOT(String fsm2dotFileName) {
		if (fsm2dot == null) {
			fsm2dot = new FSMtoDOT();
		}
		fsm2dot.apply(fsmAllInOne, fsm2dotFileName, "FSM");
	}

	public void FSM2FsmXpr(Fsm fsm, String fileName, String dirName, String ext) {
		convertFsmXpr = new ConvertFSMXpr(fsm.transitions);
		// convertFsmXpr.convertTransitions(fileName,dirName,ext); //original
		// states in terms of id
		convertFsmXpr.convertTransitions_readableLabels(fileName, dirName, ext); // more
																					// readable
																					// states
	}

	/**
	 * Added by Cu
	 * 
	 * @param fileFullName
	 */
	public void FSM2FsmXpr(String fileFullName) {
		convertFsmXpr = new ConvertFSMXpr(fsmAllInOne.transitions);
		// convertFsmXpr.convertTransitions(fileName,dirName,ext); //original
		// states in terms of id
		convertFsmXpr.convertTransitions_readableLabels(fileFullName, null,
				null); // more readable states
	}

	public Vector<Transition> getOngoingTransitions(long stateSourceId) {
		Vector<Transition> tout = new Vector<Transition>();
		for (int i = 0; i < fsmAllInOne.transitions.getTransitions().size(); i++) {
			if (fsmAllInOne.transitions.getTransitions().get(i)
					.getIdStateSource() == stateSourceId) {
				tout.add(fsmAllInOne.transitions.getTransitions().get(i));
			}
		}
		return tout;
	}

	public File[] getFilelist(String folderPath, int maxFilePemutations) {

		logseqs.setUp(folderPath, maxFilePemutations, false);

		Vector<File[]> listOfFilesSequences = logseqs.getSequenceList();

		return listOfFilesSequences.get(0);
	}

}
