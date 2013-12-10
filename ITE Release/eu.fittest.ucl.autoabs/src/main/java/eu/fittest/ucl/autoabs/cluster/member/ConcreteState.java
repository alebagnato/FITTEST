/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */

package eu.fittest.ucl.autoabs.cluster.member;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * Class representing a concrete state
 */
public class ConcreteState {

	/** concrete state values, i.e. a mix of Double and String */
	private final Object[] values;

	public ConcreteState(Object[] values) {
		this.values = values;
	}
	
	public List<BigDecimal> getDoubleValues() {
		List<BigDecimal> doubles = new ArrayList<BigDecimal>();
		for(Object o : this.values) {
			if(o instanceof BigDecimal) {
				doubles.add((BigDecimal)o);
			}
		}
		return doubles;
	}
	
	public List<String> getStringValues() {
		List<String> strings = new ArrayList<String>();
		for(Object o : this.values) {
			if(o instanceof String) {
				strings.add((String)o);
			}
		}
		return strings;
	}
	
	public Object[] getValues() {
		return this.values; 
	}
	
	private static String quoteString(String str){
		return "\"" + str.replace("\\", "\\\\").replace("\0", "\\0").replace("\b","\\b").replace("\f","\\f").
				replace("\n","\\n").replace("\r", "\\r").replace("\t", "\\t").
				replace("\\x0B", "\\\\x0B").replace("\"", "\\\"") + "\"";
	}
	private String toString(StringBuilder sb){
		int counter = 0;
		for(Object o : this.values) {
			if(counter > 0)
				sb.append(",");
			/*if(o instanceof String && ((String)o).isEmpty())
				sb.append("-");//sb.append("\"''\"");
			else*/ 
			if(o instanceof String) {
				String s = (String)o;
				if(s.isEmpty()){
					sb.append("\"\"");
				} else {
					if(!s.startsWith("\"") && !s.endsWith("\""))
						sb.append(quoteString(s));
					else
						sb.append(s);
				}
			} else
				sb.append(o.toString());
			counter++;
		}
		return sb.toString();
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		return this.toString(sb);
	}
}
