package fbk.se.mutation.json.operators;

import com.fasterxml.jackson.databind.JsonNode;
import fbk.se.mutation.constraints.OnElementType;

public interface IMutationOperator {
	public static final String XSD_MUTATION_MINEXCLUSIVE = "minExclusive";
	public static final String XSD_MUTATION_MININCLUSIVE = "minInclusive";
	public static final String XSD_MUTATION_MAXEXCLUSIVE = "maxExclusive";
	public static final String XSD_MUTATION_MAXINCLUSIVE = "maxInclusive";
	public static final String XSD_MUTATION_TOTALDIGITS = "totalDigits";
	public static final String XSD_MUTATION_FRACTIONDIGITS = "fractionDigits";
	public static final String XSD_MUTATION_LENGTH = "length";
	public static final String XSD_MUTATION_MINLENGTH = "minLength";
	public static final String XSD_MUTATION_MAXLENGTH = "maxLength";
	public static final String XSD_MUTATION_ENUMERATION = "enumeration";
	public static final String XSD_MUTATION_WHITESPACE = "whiteSpace";
	public static final String XSD_MUTATION_PATTERN = "pattern";
	
	
	public static final String[] XSD_MUTATION_TYPES = {
		XSD_MUTATION_MINEXCLUSIVE,
		XSD_MUTATION_MININCLUSIVE,
		XSD_MUTATION_MAXEXCLUSIVE,
		XSD_MUTATION_MAXINCLUSIVE,
		XSD_MUTATION_TOTALDIGITS,
		XSD_MUTATION_FRACTIONDIGITS,
		XSD_MUTATION_LENGTH,
		XSD_MUTATION_MINLENGTH,
		XSD_MUTATION_MAXLENGTH,
		XSD_MUTATION_ENUMERATION,
		XSD_MUTATION_WHITESPACE,
		XSD_MUTATION_PATTERN
	};
	
	// Mutation interface
	public JsonNode mutate(JsonNode input, OnElementType constraint);
	
}
