/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.flexdrv;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @author tiella
 */
public class StateDescription {

	public enum ValueType {
		INT, STRING, FLOAT
	}

	Map<String, String> values = new HashMap<String, String>();
	Map<String, ValueType> types = new HashMap<String, ValueType>();

	public void set(String name, ValueType t, String value) {
		values.put(name, value);
		types.put(name, t);
	}

	@Override
	public String toString() {
		return "StateDescription{" + "values=" + values + ", types=" + types + '}';
	}

	public int getAsInt(String name) {

		if (types.get(name) == ValueType.INT) {

			return Integer.parseInt(values.get(name));

		} else {
			throw new IllegalArgumentException("no such attribute or wrong type:" + name);
		}

	}

}
