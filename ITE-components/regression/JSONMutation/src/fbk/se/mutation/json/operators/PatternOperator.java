package fbk.se.mutation.json.operators;

import java.util.List;
import java.util.Random;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import dk.brics.automaton.RegExp;

import fbk.se.mutation.constraints.ComplexRestrictionType;
import fbk.se.mutation.constraints.OnElementType;
import fbk.se.mutation.constraints.Pattern;
import fbk.se.mutation.utils.Generator;
import fbk.se.mutation.xpath.VerySimpleXpathParser;

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
	public JsonNode mutate(JsonNode input, OnElementType constraint) {
		ComplexRestrictionType restriction = constraint.getRestriction();
		if (restriction == null) {
			return null;
		}
		// get the field name, last element of the path
		VerySimpleXpathParser pathParser = new VerySimpleXpathParser(
				constraint.getXpath());
		String fieldName = pathParser.getTargetFieldName();
		if (fieldName == null)
			return null;
		
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
						JsonNode mutatedNode = input.deepCopy();
						((ObjectNode)mutatedNode).put(fieldName, newContent);
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
