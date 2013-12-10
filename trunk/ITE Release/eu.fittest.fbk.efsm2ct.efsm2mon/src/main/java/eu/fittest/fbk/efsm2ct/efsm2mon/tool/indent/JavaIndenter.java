/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.tool.indent;

/**
 * 
 * @author tiella
 */
public class JavaIndenter implements Indenter {

	/**
	 * a naive indentation procedure
	 * 
	 * @param normOut
	 * @return
	 */
	public String indent(String rawOut) {

		String normOut = rawOut.replaceAll(" +", " ").replaceAll("\n+", "\n");

		StringBuilder sb = new StringBuilder();

		int indent = 0;

		for (char c : normOut.toCharArray()) {

			boolean append = true;

			switch (c) {
			case '{':
				indent++;
				break;
			case '}':
				indent--;
				sb.append('\n');
				for (int k = 0; k < indent; k++) {
					sb.append('\t');
				}
				break;
			case '\n':
				append = false;
				sb.append(c);
				for (int k = 0; k < indent; k++) {
					sb.append('\t');
				}
				break;
			}

			if (append) {
				sb.append(c);
			}

		}

		return sb.toString();

	}

}
