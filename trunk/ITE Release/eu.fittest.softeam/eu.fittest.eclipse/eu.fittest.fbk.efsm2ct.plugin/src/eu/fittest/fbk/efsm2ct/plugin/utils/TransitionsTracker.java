package eu.fittest.fbk.efsm2ct.plugin.utils;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.FsmTesterException;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.State;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Transition;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.parser.FsmParser;

public class TransitionsTracker {

	private Model model;
	private Map<Triple<String, String, String>, Boolean> targets = new HashMap<Triple<String, String, String>, Boolean>();
	private int covered = 0;

	public TransitionsTracker(String fsmFilePath) throws FileNotFoundException, FsmTesterException {

		model = FsmParser.parse(new FileReader(fsmFilePath));

		initTargets();

	}

	private void initTargets() {

		for (State s : model.getStates()) {

			for (Transition t : s.getOutgoing()) {

				State d = t.getTarget();

				targets.put(new Triple<String, String, String>(s.getName(), t.getMutator().getAlias(), d.getName()), Boolean.FALSE);

			}

		}

	}

	public boolean covered(String mutatorAlias, String sourceAlias, String targetAlias) {

		boolean res = false;

		Triple<String, String, String> target = new Triple<String, String, String>(sourceAlias, mutatorAlias, targetAlias);

		if (!targets.get(target)) {

			targets.put(target, Boolean.TRUE);
			res = true;
			covered++;

		}

		return res;

	}

	public int size() {
		return targets.size();
	}

	public int getCovered() {
		return covered;
	}
	
	public Map<Triple<String, String, String>, Boolean> getTargets() {
		return targets;
	}

	public static class Triple<T1, T2, T3> {

		private T1 c1;
		private T2 c2;
		private T3 c3;

		public Triple(T1 c1, T2 c2, T3 c3) {

			super();
			this.c1 = c1;
			this.c2 = c2;
			this.c3 = c3;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((c1 == null) ? 0 : c1.hashCode());
			result = prime * result + ((c2 == null) ? 0 : c2.hashCode());
			result = prime * result + ((c3 == null) ? 0 : c3.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Triple other = (Triple) obj;
			if (c1 == null) {
				if (other.c1 != null)
					return false;
			} else if (!c1.equals(other.c1))
				return false;
			if (c2 == null) {
				if (other.c2 != null)
					return false;
			} else if (!c2.equals(other.c2))
				return false;
			if (c3 == null) {
				if (other.c3 != null)
					return false;
			} else if (!c3.equals(other.c3))
				return false;
			return true;
		}

		@Override
		public String toString() {
			
			return "Triple [c1=" + c1 + ", c2=" + c2 + ", c3=" + c3 + "]";
		}

		public T1 getC1() {
			return c1;
		}

		public T2 getC2() {
			return c2;
		}

		public T3 getC3() {
			return c3;
		}

		
		
	}

}
