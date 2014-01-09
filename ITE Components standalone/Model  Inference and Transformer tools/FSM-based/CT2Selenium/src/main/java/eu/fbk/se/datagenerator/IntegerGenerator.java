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

import eu.fbk.se.fsm.xinput.ComplexDataSpecType;
import eu.fbk.se.fsm.xinput.Facet;

public class IntegerGenerator implements IDataGenerator {
	private static Random ranGenerator = new Random();
	
	public String generate(ComplexDataSpecType dataClz) {
		List<Object> facets =  dataClz.getFacets();
		
		Long min = Long.MIN_VALUE;
		Long max = Long.MAX_VALUE;
		boolean minPresent = false;
		boolean maxPresent = false;
		
		for (Object o : facets) {
			// Find the facet that specify the min/max inclusive value
			if (o instanceof JAXBElement) {
				JAXBElement tmp = (JAXBElement)o;
				String constraintType = tmp.getName().getLocalPart();
				if (constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MINEXCLUSIVE)
						|| constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MININCLUSIVE)) {
					// treat these two type equally
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					try {
						min = Long.valueOf(value);
						minPresent = true;
					} catch (NumberFormatException e) {
						e.printStackTrace();
					}
				}

				if (constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MAXEXCLUSIVE)
						|| constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MAXINCLUSIVE)) {
					// treat these two type equally
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					try {
						max = Long.valueOf(value);
						maxPresent = true;
					} catch (NumberFormatException e) {
						e.printStackTrace();
					}
				}
			}
			
			if (minPresent && maxPresent){
				return String.valueOf(min.longValue() + (long)(ranGenerator.nextDouble() 
							* (max.doubleValue() - min.doubleValue())));
			}
		}
		
		double DELTA = 1;
		
		if (minPresent){
			return String.valueOf(min.longValue() + (long)(ranGenerator.nextDouble() * min.doubleValue()) + DELTA);
		} 
		
		if (maxPresent){
			return String.valueOf((long)(ranGenerator.nextDouble() * max.doubleValue()) - DELTA); 
		}
					
		return "0";
	}

}
