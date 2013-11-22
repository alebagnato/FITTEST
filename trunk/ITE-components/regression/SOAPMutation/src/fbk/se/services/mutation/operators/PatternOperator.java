package fbk.se.services.mutation.operators;

import java.util.List;
import java.util.Random;

import org.w3c.dom.Node;

import dk.brics.automaton.RegExp;

import fbk.se.services.mutation.constraints.ComplexRestrictionType;
import fbk.se.services.mutation.constraints.OnElementType;
import fbk.se.services.mutation.constraints.Pattern;
import fbk.se.services.mutation.utils.Generator;

/**
 * Mutate an input node of type String based on its constraint specified
 * using regular expression.
 * 
 * @author cdnguyen
 *
 */
public class PatternOperator implements IMutationOperator {
	
	private static final String REG_EX1 = "[0-9]";
	private static final String REG_EX2 = "\\S"; // any character except white spaces 
	private static final String REG_EX3 = "[a-zA-z]";
	private static final String REG_EX4 = "\\w+"; // any character in the range 0 - 9, A - Z and a - z 
	private static final String REG_EX5 = "[0-9]+";
	private static final String REG_EX6 = "[a-z]{2}";
	
	private static final String[] SIMPLE_REG_LIBS = {
		REG_EX1,REG_EX2,REG_EX3,REG_EX4,REG_EX5,REG_EX6};
	
	private static final Random ranSelector = new Random();
	
	@Override
	public Node mutate(Node input, OnElementType constraint) {
		ComplexRestrictionType restriction = constraint.getRestriction();
		if (restriction == null) {
			return null;
		}

		String base = restriction.getBase().getLocalPart();

		List<Object> facets = restriction.getFacets();
		for (Object o : facets) {
			if (o instanceof Pattern) {
				if (base.equalsIgnoreCase("string")) {
					// String only
					Pattern pattern = (Pattern) o;
					String regex = pattern.getValue();

					// mutate the expression
					regex = mutateRegEx(regex);

					if (regex != null) {
						// generate a new string that satisfies the RegEx
						String newContent = Generator.generateRegexString(regex);
						Node mutatedNode = input.cloneNode(true);
						Node firstChild = mutatedNode.getFirstChild();
						if (firstChild != null) {
							firstChild.setTextContent(newContent);
						}
						return mutatedNode;
					}

				} else {
					// ignore
					return null;
				}
			}
		}

		return null;
	}

	/**
	 * Mutate the regular expression
	 * 
	 * @param originalRegEx
	 * @return
	 */
	private String mutateRegEx(String originalRegEx) {
		try {
			// parsing the current expression to check for validity
			RegExp parser = new RegExp(originalRegEx);
			
			// Concatenate with a simple regular expression to mutate
			String newRegEx = originalRegEx 
						+ SIMPLE_REG_LIBS[ranSelector.nextInt(SIMPLE_REG_LIBS.length)];
			return newRegEx;
		} catch (IllegalArgumentException e) {
			// fail to parse the expression
			e.printStackTrace();
			return null;
		}
	}
}
