package eu.fittest.fbk.efsm2ct.log2efsm.logconv.model;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.TypedValue;

/**
 * Model an instance of an "Object" in a log file
 * 
 * @author tiella
 * 
 */

public class Instance {

	private String type;
	private Map<String, TypedValue> attrBindings = new HashMap<String, TypedValue>();

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public int size() {
		return attrBindings.size();
	}

	public boolean isEmpty() {
		return attrBindings.isEmpty();
	}

	public boolean containsKey(Object key) {
		return attrBindings.containsKey(key);
	}

	public TypedValue get(Object key) {
		return attrBindings.get(key);
	}

	public TypedValue put(String key, TypedValue value) {
		return attrBindings.put(key, value);
	}

	public Set<String> keySet() {
		return attrBindings.keySet();
	}

	public void dump(String tabs) {

		System.out.println("\tinstance type:" + type);

		tabs += "\t";

		for (Map.Entry<String, TypedValue> e : attrBindings.entrySet()) {

			System.out.println(tabs + "attr:" + e.getKey());
			System.out.println(tabs + "\ttype:" + e.getValue().getType());
			System.out.println(tabs + "\tvalue:" + e.getValue().getValue());

		}

	}

	@Override
	public String toString() {

		StringBuilder sb = new StringBuilder();

		sb.append("instance [");
		sb.append(type);
		sb.append("] {");

		for (Map.Entry<String, TypedValue> e : attrBindings.entrySet()) {

			sb.append(e.getKey());
			sb.append("[").append(e.getValue().getType()).append("]");
			sb.append("=").append(e.getValue().getValue());
			sb.append(",");

		}

		sb.delete(sb.length() - 1, sb.length());

		sb.append("}");

		return sb.toString();
	}

}
