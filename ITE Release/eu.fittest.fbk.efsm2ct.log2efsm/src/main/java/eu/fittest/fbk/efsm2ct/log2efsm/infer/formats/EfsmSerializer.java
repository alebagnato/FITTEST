package eu.fittest.fbk.efsm2ct.log2efsm.infer.formats;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.Fsm;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.State;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.States;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.Transition;
import eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm.Transitions;

/**
 * exports a FSM in efsm format
 * 
 * @param <K>
 */

public class EfsmSerializer {

	private Map<String, String> mutatorAliasTable = new HashMap<String, String>();
	private Map<String, String> inspectorAliasTable = new HashMap<String, String>();

	private Map<Long, String> stateNames = new HashMap<Long, String>();
	private Map<Long, String> statePredicates = new HashMap<Long, String>();
	private Map<String, String> comparatorTable = new HashMap<String, String>();

	// private Map<String, String> mutatorSignatureTable = new HashMap<String,
	// String>();

	private PrintStream out;
	private String packageName;
	private String className;
	private MutatorDescriptionSet mds;

	private List<Long> removedStates = new ArrayList<Long>();

	public EfsmSerializer(String packageName, String className, MutatorDescriptionSet mds, String fileName) throws IOException {

		this.packageName = packageName;
		this.className = className;

		out = new PrintStream(new File(fileName));

		// TODO: these must be read from state abstraction configuration file
		comparatorTable.put("Zero", " == 0");
		comparatorTable.put("GreaterThanZero", " > 0");
		comparatorTable.put("Empty", " == -1");
		comparatorTable.put("NotEmpty", " >= 0");

		this.mds = mds;

		init();

	}

	/**
	 * 
	 */
	private void init() {

		for (String name : mds.nameSet()) {

			MutatorDescriptor md = mds.getByName(name);

			String alias = String.format("m%d", md.getId());

			mutatorAliasTable.put(name, alias);

		}

	}

	public void export(Fsm fsm) {

		States states = fsm.states;
		Transitions transitions = fsm.transitions;

		removedStates.clear();

		collectStatesInformation(states);

		out.format("fsm %s.%s {\n", packageName, className);

		out.println();

		exportMutatorSection();

		out.println();

		exportInspectorsSection();

		out.println();

		exportStatesSection();

		out.println();

		exportTransitionsSection(transitions);

		out.println();

		out.println("}");

		out.close();

	}

	private void exportMutatorSection() {

		out.println("mutators {");

		for (String name : mds.nameSet()) {

			MutatorDescriptor md = mds.getByName(name);

			String alias = mutatorAliasTable.get(name);

			out.format("%s := %s ;\n", alias, md.getSignature());

		}

		out.println("}");
	}

	private void exportInspectorsSection() {

		out.println("inspectors {");

		for (String ins : inspectorAliasTable.keySet()) {

			out.format("int %s := %s() ;\n", inspectorAliasTable.get(ins), ins);

		}

		out.println("}");

	}

	private void exportStatesSection() {

		out.println("states {");

		for (long id : stateNames.keySet()) {

			// perform three fixes to the FSM: TODO it would be better to clean
			// the model, i.e. the FSM instance, but
			// here it's simple at the moment

			String sn = stateNames.get(id);
			String sp = statePredicates.get(id);
			String init = "";

			// 1. state named n0 must be removed
			if (sn.equals("n0")) {

				removedStates.add(id);
				continue;

			}

			// 2. state named n1 must contain predicate 'false'
			if (sn.equals("n1")) {
				init = "[initial] ";
				sp = "false";

			}

			// 3. remove state with predicate "end"

			if (sp.equals("end")) {

				removedStates.add(id);
				continue;

			}

			out.format("%s %s{ %s ; };\n", sn, init, sp);

		}

		out.println("}");
	}

	private void exportTransitionsSection(Transitions transitions) {

		out.println("transitions {");

		for (Transition t : transitions.getTransitions()) {

			// if a transition's source or target node was removed, the
			// transition has to be removed too
			if (removedStates.contains(t.getIdStateSource()) || removedStates.contains(t.getIdStateTarget())) {
				continue;
			}

			String eventName = t.getTransitionContent()[0];

			String alias = mutatorAliasTable.get(eventName);

			if (alias == null) {
				continue;
			}

			out.format("%s -> %s { %s; };\n", stateNames.get(t.getIdStateSource()), stateNames.get(t.getIdStateTarget()), alias);

		}

		out.println("}");
	}

	private void collectStatesInformation(States states) {

		int snum = 0;

		for (State s : states.getStates()) {

			String sname = String.format("n%01d", snum);

			stateNames.put(s.getId(), sname);

			String predicate = mapPredicate(s.getStateContent());

			statePredicates.put(s.getId(), predicate);

			snum++;

		}
	}

	private String mapPredicate(String[] stateContent) {

		// System.out.println("working on:"+Arrays.toString(stateContent));

		StringBuffer sb = new StringBuffer();

		if (stateContent.length == 1 && (stateContent[0].equals("start") || stateContent[0].equals("undef") || stateContent[0].equals("end"))) {

			sb.append(stateContent[0]);

		} else {

			int k = 0;

			int insNum = 1;

			for (String a : stateContent) {

				String[] exp = a.split("=");
				String[] var = exp[0].split(":");

				String inspector = var[0];

				String alias = inspectorAliasTable.get(inspector);

				if (alias == null) {

					alias = String.format("s%d", insNum);
					insNum++;

					inspectorAliasTable.put(inspector, alias);

				}

				sb.append(alias);

				sb.append(comparatorTable.get(exp[1]));

				if (k < stateContent.length - 1) {

					sb.append(" && ");

				}

				k++;

			}
		}

		return sb.toString();

	}

}
