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

import com.stromberglabs.cluster.UniPoint;

public class NumericData extends LogData {
	
	public NumericData(String dataType) {
		super();
		this.dataType = dataType;
	}
	
//	public float getLowerBoundary(){
//		float min = Float.MAX_VALUE;
//		for (UniPoint p : getEntries(UniPoint.class)){
//			if (p.getValue() < min)
//				min = p.getValue();
//		}
//		return min;
//	}
//
//	public float getUpperBoundary(){
//		float max = Float.MIN_VALUE;
//		for (UniPoint p : getEntries(UniPoint.class)){
//			if (p.getValue() > max)
//				max = p.getValue();
//		}
//		return max;
//	}

	@Override
	public void add(String value) {
		float v = Float.valueOf(value).floatValue();
		UniPoint point = new UniPoint(v);
		if (!getEntries().contains(point))
			getEntries().add(point);
	}
	
}
