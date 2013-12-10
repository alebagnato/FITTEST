package eu.fbk.se.datagenerator;

import eu.fbk.se.fsm.xinput.ComplexDataSpecType;

public interface IDataGenerator {
	
	public static final String DOMAIN_CONSTRAINT_MINEXCLUSIVE = "minExclusive";
	public static final String DOMAIN_CONSTRAINT_MININCLUSIVE = "minInclusive";
	public static final String DOMAIN_CONSTRAINT_MAXEXCLUSIVE = "maxExclusive";
	public static final String DOMAIN_CONSTRAINT_MAXINCLUSIVE = "maxInclusive";
	public static final String DOMAIN_CONSTRAINT_TOTALDIGITS = "totalDigits";
	public static final String DOMAIN_CONSTRAINT_FRACTIONDIGITS = "fractionDigits";
	public static final String DOMAIN_CONSTRAINT_LENGTH = "length";
	public static final String DOMAIN_CONSTRAINT_MINLENGTH = "minLength";
	public static final String DOMAIN_CONSTRAINT_MAXLENGTH = "maxLength";
	public static final String DOMAIN_CONSTRAINT_ENUMERATION = "enumeration";
	public static final String DOMAIN_CONSTRAINT_WHITESPACE = "whiteSpace";
	public static final String DOMAIN_CONSTRAINT_PATTERN = "pattern";
	
	
	public static final String[] DOMAIN_CONSTRAINT_TYPES = {
		DOMAIN_CONSTRAINT_MINEXCLUSIVE,
		DOMAIN_CONSTRAINT_MININCLUSIVE,
		DOMAIN_CONSTRAINT_MAXEXCLUSIVE,
		DOMAIN_CONSTRAINT_MAXINCLUSIVE,
		DOMAIN_CONSTRAINT_TOTALDIGITS,
		DOMAIN_CONSTRAINT_FRACTIONDIGITS,
		DOMAIN_CONSTRAINT_LENGTH,
		DOMAIN_CONSTRAINT_MINLENGTH,
		DOMAIN_CONSTRAINT_MAXLENGTH,
		DOMAIN_CONSTRAINT_ENUMERATION,
		DOMAIN_CONSTRAINT_WHITESPACE,
		DOMAIN_CONSTRAINT_PATTERN
	};
	
	public String generate(ComplexDataSpecType dataClz);
}
