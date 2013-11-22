package com.stromberglabs.cluster;

public class UniPoint implements Clusterable {

	private float x;
	
	@Override
	public float[] getLocation() {
		return new float[]{x,0};
	}

	public UniPoint(float x) {
		this.x = x;
	}
	
	public float getValue(){
		return x;
	}

	@Override
	public String toString() {
		return String.valueOf(x);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof UniPoint && x == ((UniPoint) obj).getValue())
			return true;
		return false;
	}
	
	
}
