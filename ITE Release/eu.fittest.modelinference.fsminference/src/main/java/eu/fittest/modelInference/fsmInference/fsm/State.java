package eu.fittest.modelInference.fsmInference.fsm;

import eu.fittest.modelInference.fsmInference.utility.Utility;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class State {
	long id = -1;
	String[] stateContent;
	Utility utils = new Utility();
	Unique uniqueIds = Unique.getInstace();

	public State(String[] stateContent) {

		long tmp = 0;
		tmp = utils.randomLong();
		while (uniqueIds.iscontainedState(tmp)) {
			tmp = utils.randomLong();
		}
		uniqueIds.addToUniqueIdStates(tmp);
		setId(tmp);
		setStateContent(stateContent);
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String[] getStateContent() {
		return stateContent;
	}

	public void setStateContent(String[] stateContent) {
		this.stateContent = utils.copy(stateContent);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (this == null)
			return false;
		if (!(o instanceof State))
			return false;
		boolean eq = false;
		if (((State) o).getId() == this.getId())
			eq = true;
		else
			eq = false;
		String[] stO = ((State) o).getStateContent();
		String[] stThis = this.getStateContent();
		if (stO.length != stThis.length)
			eq = false;
		else {
			for (int i = 0; i < stO.length; i++) {
				if (stO[i].equals(stThis[i]))
					eq = true;
				else {
					eq = false;
					break;
				}
			}
		}
		return eq;
	}

	@Override
	public State clone() {
		String[] ttr = utils.copy(this.stateContent);
		State clone = new State(ttr);
		clone.setId(this.id);
		return clone;
	}
}
