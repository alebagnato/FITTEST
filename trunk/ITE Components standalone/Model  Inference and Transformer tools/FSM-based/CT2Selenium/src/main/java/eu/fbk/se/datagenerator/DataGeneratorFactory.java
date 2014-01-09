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

public class DataGeneratorFactory {
	public static IDataGenerator createDataGenerator(ComplexDataSpecType dataClz){
		if ("double".equals(dataClz.getBase().getLocalPart())
				|| "decimal".equals(dataClz.getBase().getLocalPart())){
			return new DoubleGenerator();
		} 

		if ("integer".equals(dataClz.getBase().getLocalPart())
				|| "long".equals(dataClz.getBase().getLocalPart())
				|| "short".equals(dataClz.getBase().getLocalPart())
				|| "int".equals(dataClz.getBase().getLocalPart())){
			return new IntegerGenerator();
		} 

		if ("string".equals(dataClz.getBase().getLocalPart())){
			return new StringGenerator();
		} 
		
		if ("date".equals(dataClz.getBase().getLocalPart())){
			return new DateGenerator();
		}

		if ("boolean".equals(dataClz.getBase().getLocalPart())){
			return new BooleanGenerator();
		}
		
		return null;
	}
}
