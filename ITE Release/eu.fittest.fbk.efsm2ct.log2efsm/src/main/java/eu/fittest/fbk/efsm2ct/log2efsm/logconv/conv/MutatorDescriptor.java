package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

/**
 * models a mutator message in terms of target object, event sent to the object
 * and event arity
 * 
 * @author tiella
 */

public class MutatorDescriptor {

	private String target;
	private String event;
	private int arity;

	public MutatorDescriptor(String target, String event, int arity) {
		super();
		this.target = target;
		this.event = event;
		this.arity = arity;
	}

	public String getTarget() {
		return target;
	}

	public String getEvent() {
		return event;
	}

	public int getArity() {
		return arity;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + arity;
		result = prime * result + ((event == null) ? 0 : event.hashCode());
		result = prime * result + ((target == null) ? 0 : target.hashCode());
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
		MutatorDescriptor other = (MutatorDescriptor) obj;
		if (arity != other.arity)
			return false;
		if (event == null) {
			if (other.event != null)
				return false;
		} else if (!event.equals(other.event))
			return false;
		if (target == null) {
			if (other.target != null)
				return false;
		} else if (!target.equals(other.target))
			return false;
		return true;
	}

	public String getSignature() {

		StringBuilder sb = new StringBuilder();

		sb.append(getName());
		sb.append(getArgs());

		return sb.toString();
	}

	public String getArgs() {

		StringBuilder sb = new StringBuilder();

		// sb.append("(");

		for (int i = 0; i < arity; i++) {

			sb.append("int");

			if (i < arity - 1) {
				sb.append(", ");
			}

		}

		// sb.append(")");

		return sb.toString();
	}

	public String getName() {

		StringBuilder sb = new StringBuilder();

		sb.append(getTarget());
		sb.append("_");
		sb.append(getEvent());

		return sb.toString();
	}

	public Object getParameters() {
		// TODO Auto-generated method stub
		return null;
	}

}
