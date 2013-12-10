/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.eventSequenceGenerator.utility;

/**
*
* @author Alessandro
*
*/
public class UtilityTracer {

	 public static String convert(String[] stateVaraibles){
		  String str="";
		  for (int i = 0; i < stateVaraibles.length; i++) {
			  if (i==0) {
				  if (!stateVaraibles[i].equals("")) 
					  str=stateVaraibles[i];
			  }
			  else {
				  if ((!stateVaraibles[i].equals(""))&&(!str.equals(""))) 
					  str=str+";__;"+stateVaraibles[i];
				  else if ((!stateVaraibles[i].equals(""))&&(str.equals("")))
					  str=stateVaraibles[i];
			  }
			
		}
		  return str;
	  }
	
}
