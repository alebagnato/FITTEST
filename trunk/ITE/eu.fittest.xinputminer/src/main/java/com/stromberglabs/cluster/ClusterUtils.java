/*
Copyright (c) 2010, Andrew Stromberg
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither Andrew Stromberg nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Andrew Stromberg BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.stromberglabs.cluster;

import java.util.List;

import com.stromberglabs.cluster.Clusterable;

public class ClusterUtils {
	public static double getEuclideanDistance(double[] a,double[] b){
		if ( a.length != b.length ){
			throw new RuntimeException("Attempting to compare two clusterables of different dimensions");
		}

		double sum = 0;
		for ( int i = 0; i < a.length; i++ ){
			double diff = a[i] - b[i];
			sum += diff*diff; 
		}
		return Math.sqrt(sum);
	}
	
	public static double getEuclideanDistance(float[] a,float[] b){
		if ( a.length != b.length ){
			throw new RuntimeException("Attempting to compare two clusterables of different dimensions");
		}

		double sum = 0;
		for ( int i = 0; i < a.length; i++ ){
			double diff = a[i] - b[i];
			sum += diff*diff; 
		}
		return Math.sqrt(sum);
	}
	
	public static double getEuclideanDistance(Clusterable a,Clusterable b){
		return getEuclideanDistance(a.getLocation(),b.getLocation());
	}
	
	public static double sumDifferences(List<Double> a, List<Double> b){
		assert(a.size() == b.size());
		double sumDiff = 0;
		double aSum = 0;
		double bSum = 0;
		for ( int i = 0; i < a.size(); i++ ){
			sumDiff += Math.abs(a.get(i) - b.get(i));
			aSum += a.get(i);
			bSum += b.get(i);
		}
		return sumDiff;
	}
	
	/**
	 * get lower boundary of a cluster 
	 * @param cluster
	 * @return
	 */
	public static UniPoint getMin(Cluster cluster){
		float min = Float.MAX_VALUE;
		UniPoint ret = new UniPoint(min);
		for (Clusterable elem : cluster.getItems()){
			if (elem instanceof UniPoint && ((UniPoint) elem).getValue() < min){
				min = ((UniPoint) elem).getValue();
				ret = (UniPoint) elem;
			}
		}
		return ret;
	}
	
	/**
	 * Get upper boundary of a cluster
	 * @param cluster
	 * @return
	 */
	public static UniPoint getMax(Cluster cluster){
		float max = Float.MIN_VALUE;
		UniPoint ret = new UniPoint(max);
		for (Clusterable elem : cluster.getItems()){
			if (elem instanceof UniPoint && ((UniPoint) elem).getValue() > max){
				max = ((UniPoint) elem).getValue();
				ret = (UniPoint) elem;
			}
		}
		return ret;
	}
	
}
