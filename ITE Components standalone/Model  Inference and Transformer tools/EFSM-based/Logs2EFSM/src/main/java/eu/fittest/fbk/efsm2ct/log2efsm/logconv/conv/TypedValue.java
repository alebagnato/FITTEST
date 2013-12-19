package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

public class TypedValue {

	Pair<Object, String> delegate;

	public TypedValue(Object value, String typeName) {

		delegate = new Pair<Object, String>();
		delegate.setFirst(value);
		delegate.setSecond(typeName);

	}

	public Object getValue() {

		Object res = null;

		if (getType().equals("String")) {

			String tmp = (String) delegate.getFirst();
			res = tmp.substring(1, tmp.length() - 1);

		} else {
			res = delegate.getFirst();
		}

		return res;
	}

	public void setValue(Object first) {
		delegate.setFirst(first);
	}

	public String getType() {
		return delegate.getSecond();
	}

	public void setType(String second) {
		delegate.setSecond(second);
	}

	public String toString() {
		return delegate.toString();
	}

}
