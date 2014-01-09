/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.modelInference.logConverter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Vector;

/**
 * It detects the abstract states. 
 * It takes a pair: field type - field value;
 * 
 * @author Alessandro Marchetto
 *
 */
public class ABSstateDetection {
	ABS abs=new ABS();

	String absCheckerAndSelection(String ty, String value) {
		try{
		String type=ty.trim().toLowerCase();
		if (abs.absList.get(type)==null) return null;
		
		Vector<String[]> vectOfCheckerXType=abs.absList.get(type);
		
		Pattern patt;
		Matcher matcher;
		
		for (String[] absState : vectOfCheckerXType) {
			//System.out.print("'"+absState[0]+"' vs '"+value+"'");
			 
			patt=Pattern.compile(absState[0],Pattern.CASE_INSENSITIVE | Pattern.CANON_EQ | Pattern.UNICODE_CASE);
			matcher = patt.matcher(value);
			
			if(matcher.matches()){
				//System.out.print(" ->yes \n");
				return absState[1];
			}
			//else  System.out.print(" \n");
		}
		
		return null;
		
		}catch(Exception e){
			e.printStackTrace();
			return null;
			}
		} 
	
	
}
