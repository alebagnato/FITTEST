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
