/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.xinputmining.logdata;

public class StringData extends LogData {

	public StringData() {
		super();
		dataType = DATA_TYPE_STRING;
	}
	
	@Override
	public void add(String value) {
		if (!getEntries().contains(value))
			getEntries().add(value);
	}
	
	public int getMaxLength(){
		int maxLen = Integer.MIN_VALUE;
		for (Object obj : getEntries()){
			if (maxLen < obj.toString().length()){
				maxLen = obj.toString().length();
			}
		}
		return maxLen;
	}

	public int getMinLength(){
		int minLen = Integer.MAX_VALUE;
		for (Object obj : getEntries()){
			if (minLen > obj.toString().length()){
				minLen = obj.toString().length();
			}
		}
		
		return minLen;
		
	}
}
