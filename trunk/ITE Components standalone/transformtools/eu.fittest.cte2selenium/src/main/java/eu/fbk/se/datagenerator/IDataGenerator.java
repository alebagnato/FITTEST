/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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
