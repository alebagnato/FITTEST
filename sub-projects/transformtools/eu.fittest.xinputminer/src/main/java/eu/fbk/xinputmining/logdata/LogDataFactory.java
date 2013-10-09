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

import eu.fbk.xinputmining.MiningException;

public class LogDataFactory {
	public static LogData createLogData(String type) throws MiningException{
		if (type.equals(LogData.DATA_TYPE_STRING)){
			return new StringData();
		} else if (type.equals(LogData.DATA_TYPE_INT)){
			return new NumericData(LogData.DATA_TYPE_INT);
		} else if (type.equals(LogData.DATA_TYPE_FLOAT)){
			return new NumericData(LogData.DATA_TYPE_FLOAT);
		} else {
			throw new MiningException("Data type: " + type + " is not supported!");
		}
	}
}
