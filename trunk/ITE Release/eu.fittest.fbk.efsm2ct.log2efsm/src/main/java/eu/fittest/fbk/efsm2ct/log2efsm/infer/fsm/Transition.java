package eu.fittest.fbk.efsm2ct.log2efsm.infer.fsm;

import eu.fittest.fbk.efsm2ct.log2efsm.infer.utility.Utility;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Transition {
	Utility utils = new Utility();

	Unique uniqueIds = Unique.getInstace();

	long idTransition = -1;
	long idStateSource = -1;
	long idStateTarget = -1;
	String[] transition;
	int transitionExecIndex = -1;
	long execFreq = 0;

	public Transition(long idStateSource, long idStateTarget) {

		long tmp = 0;
		tmp = utils.randomLong();
		while (uniqueIds.iscontainedState(tmp)) {
			tmp = utils.randomLong();
		}
		uniqueIds.addToUniqueIdTransitions(tmp);
		setidTransition(tmp);
		setIdStateSource(idStateSource);
		setIdStateTarget(idStateTarget);
		setExcFreqTransition(1);
	}

	public long getidTransition() {
		return this.idTransition;
	}

	public long getExcFreqTransition() {
		return this.execFreq;
	}

	public long setExcFreqTransition(long freq) {
		return this.execFreq = freq;
	}

	public void setidTransition(long idTransition) {
		this.idTransition = idTransition;
	}

	public String[] getTransitionContent() {
		return transition;
	}

	public void setTransitionContent(String[] transition) {
		this.transition = utils.copy(transition);
	}

	public int getTransitionExecIndex() {
		return transitionExecIndex;
	}

	public void setTransitionExecIndex(int transitionExecIndex) {
		this.transitionExecIndex = transitionExecIndex;
	}

	@Override
	public boolean equals(Object o) {
		if (this == null)
			return false;
		if (!(o instanceof Transition))
			return false;
		boolean eq = false;

		String[] stO = ((Transition) o).getTransitionContent();
		String[] stThis = this.getTransitionContent();
		for (int i = 0; i < stO.length; i++) {
			if (!stO[i].equals(stThis[i]))
				return false;
			else {
				eq = true;
			}
		}

		if (eq == true) {
			if (this.getIdStateSource() == ((Transition) o).getIdStateSource()) {
				if (this.getidTransition() == ((Transition) o).getidTransition()) {
					eq = true;
					return true;
				}
				return false;
			}
			return false;
		}

		return eq;
	}

	public long getIdStateSource() {
		return idStateSource;
	}

	public void setIdStateSource(long idStateSource) {
		this.idStateSource = idStateSource;
	}

	public long getIdStateTarget() {
		return idStateTarget;
	}

	public void setIdStateTarget(long idStateTarget) {
		this.idStateTarget = idStateTarget;
	}

	@Override
	public Transition clone() {
		Transition clone = new Transition(this.idStateSource, this.idStateTarget);
		clone.idTransition = this.idTransition;
		clone.execFreq = this.execFreq;
		clone.transitionExecIndex = this.transitionExecIndex;
		String[] ttr = utils.copy(this.transition);
		clone.setTransitionContent(ttr);
		return clone;
	}

}
