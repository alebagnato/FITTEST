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

import java.util.List;
import java.util.Random;

import javax.xml.bind.JAXBElement;

import nl.flotsam.xeger.Xeger;

import eu.fbk.se.fsm.xinput.ComplexDataSpecType;
import eu.fbk.se.fsm.xinput.Facet;
import eu.fbk.se.fsm.xinput.Pattern;
import eu.fbk.se.utils.Constants;

public class StringGenerator implements IDataGenerator {

	private static String TOKENS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
	private static Random ranGenerator = new Random();
	
	/**
	 * Generate randomly a string of length: len
	 * @param len
	 * @return
	 */
	private String generateString(int len){
		String ret = "";//= Constants.STRING_QUOTE;
		for (int i = 0; i < len; i++) {
			ret += TOKENS.charAt(ranGenerator.nextInt(TOKENS.length()));
		}
		return ret; // + Constants.STRING_QUOTE;
	}
	
	public String generate(ComplexDataSpecType dataClz) {
		List<Object> facets =  dataClz.getFacets();
		
		for (Object o : facets) {
			// Find the facet that specify the min, max, or len, or pattern value
			if (o instanceof JAXBElement) {
				JAXBElement tmp = (JAXBElement)o;
				String constraintType = tmp.getName().getLocalPart();
				if (constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MINLENGTH)) {
					// treat these two type equally
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					try {
						int min = Integer.valueOf(value).intValue();
						return generateString(min + 1);
					} catch (NumberFormatException e) {
						e.printStackTrace();
					}
				}

				if (constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MAXLENGTH)
						|| constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_LENGTH)) {
					// treat these two type equally
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					try {
						int max = Integer.valueOf(value).intValue();
						return generateString(max);
					} catch (NumberFormatException e) {
						e.printStackTrace();
					}
				}
			}
			
			if (o instanceof Pattern){
				Pattern pattern = (Pattern) o;
				String regex = pattern.getValue();

				// generate a new string that satisfies the RegEx
				return generateRegexString(regex);
			}
		}
		
		return dataClz.getName();
	}

	
	/**
	 * Generate a string that matches a regular expression
	 * @param regex
	 * @return
	 */
	private String generateRegexString(String regex){
		Xeger generator = new Xeger(regex);
		String result = generator.generate();
		return result;
	}
}
