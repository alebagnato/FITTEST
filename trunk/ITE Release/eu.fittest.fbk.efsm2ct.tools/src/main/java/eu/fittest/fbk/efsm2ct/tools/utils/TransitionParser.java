package eu.fittest.fbk.efsm2ct.tools.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TransitionParser {

	// log entry example ---> "INFO: TRANSITION: m2 : n1 -> n2"

	private static Pattern entryPattern = Pattern.compile(".*TRANSITION: ([a-zA-Z0-9]+) : ([a-zA-Z0-9]+) -> ([a-zA-Z0-9]+)");

	private String mutator;
	private String sourceState;
	private String destState;

	public boolean match(String line) {

		boolean res = false;

		Matcher matcher = entryPattern.matcher(line);

		if (matcher.matches()) {
			res = true;
			mutator = matcher.group(1);
			sourceState = matcher.group(2);
			destState = matcher.group(3);

		} else {
			mutator = null;
			sourceState = null;
			destState = null;
		}

		return res;
	}

	public String getMutator() {
		return mutator;
	}

	public String getSourceState() {
		return sourceState;
	}

	public String getDestState() {
		return destState;
	}

}
