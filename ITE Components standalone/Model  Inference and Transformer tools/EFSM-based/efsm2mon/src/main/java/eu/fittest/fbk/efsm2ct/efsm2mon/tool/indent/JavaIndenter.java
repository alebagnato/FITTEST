/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

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
