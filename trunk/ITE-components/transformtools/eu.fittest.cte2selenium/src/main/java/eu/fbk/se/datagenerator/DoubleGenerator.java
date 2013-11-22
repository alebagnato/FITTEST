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

import java.text.NumberFormat;
import java.util.List;
import java.util.Locale;
import java.util.Random;

import javax.xml.bind.JAXBElement;

import eu.fbk.se.fsm.xinput.ComplexDataSpecType;
import eu.fbk.se.fsm.xinput.Facet;

public class DoubleGenerator implements IDataGenerator {
	private static Random ranGenerator = new Random();
	
	public String generate(ComplexDataSpecType dataClz) {
		List<Object> facets =  dataClz.getFacets();
		
		NumberFormat formater = NumberFormat.getNumberInstance(Locale.getDefault());
		formater.setMaximumFractionDigits(2);
		formater.setMinimumFractionDigits(2);
	    
		boolean minPresent = false;
		boolean maxPresent = false;
		Double min = Double.MIN_VALUE;
		Double max = Double.MAX_VALUE;
		
		double retVal = 0.00;
		for (Object o : facets) {
			// Find the facet that specify the min value
			if (o instanceof JAXBElement) {
				JAXBElement tmp = (JAXBElement)o;
				String constraintType = tmp.getName().getLocalPart();
				if (constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MINEXCLUSIVE)
						|| constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MININCLUSIVE)) {
					// treat these two type equally
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					try {
						min = Double.valueOf(value);
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
						max = Double.valueOf(value);
						maxPresent = true;
					} catch (NumberFormatException e) {
						e.printStackTrace();
					}
				}
			}
			
			if (minPresent && maxPresent){
				retVal = min.doubleValue() + ranGenerator.nextDouble() 
						* (max.doubleValue() - min.doubleValue());
			}
		}
		double DELTA = 0.01;
		
		if (minPresent){
			retVal = min.doubleValue() + ranGenerator.nextDouble() * min.doubleValue() + DELTA;
		} 
		
		if (maxPresent){
			retVal = ranGenerator.nextDouble() * max.doubleValue() - DELTA; 
		}
					
		return formater.format(retVal);
	}

}
