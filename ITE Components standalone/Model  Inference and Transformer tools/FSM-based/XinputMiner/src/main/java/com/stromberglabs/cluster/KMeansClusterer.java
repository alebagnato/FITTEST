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

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import com.stromberglabs.cluster.Clusterable;

public class KMeansClusterer extends AbstractKClusterer {
	public KMeansClusterer() {
		super();
	}
	
	protected Cluster[] assignClusters(Cluster[] clusters,final List<? extends Clusterable> values){
		assignClustersByDistance(clusters, values);
		return clusters;
	}
	
	protected void assignClustersByDistance(Cluster[] clusters, List<? extends Clusterable> values){
		for ( int j = 0; j < values.size(); j++ ){
			Clusterable val = values.get(j);
			Cluster nearestCluster = null;
			double minDistance = Float.MAX_VALUE;
			for ( int i = 0; i < clusters.length; i++ ){
				Cluster cluster = clusters[i];
				double distance = ClusterUtils.getEuclideanDistance(val,cluster);
				//System.out.println("cluster " + i + ", point " + j + ",distance: " + distance);
				if ( distance < minDistance ){
					nearestCluster = cluster;
					minDistance = distance;
				}
			}
			nearestCluster.addItem(val);
		}
	}
	
	protected Cluster[] getNewClusters(Cluster[] clusters){
		for ( int i = 0; i < clusters.length; i++ ){
			if ( clusters[i].getItems().size() > 0 )
				clusters[i] = new Cluster(clusters[i].getClusterMean(),i);
		}
		return clusters;
	}
	
	/**
	 * For testing purpose
	 * @param args
	 */
	public static void main(String args[]){
		List<Clusterable> points = new ArrayList<Clusterable>();
		
		points.add(new UniPoint(1));
		points.add(new UniPoint(2));
		points.add(new UniPoint(3));
		points.add(new UniPoint(2));
		points.add(new UniPoint(1));

		points.add(new UniPoint(10));
		points.add(new UniPoint(9));
		points.add(new UniPoint(11));
		points.add(new UniPoint(12));
		points.add(new UniPoint(10));
		points.add(new UniPoint(11));
		
		int size = (int) Math.sqrt((double)points.size() / 2.0);
		KClusterer clusterer = new KMeansClusterer();
		Cluster[] clusters = clusterer.cluster(points,size);
		for ( Cluster c : clusters ){
			System.out.println(c.getItems().size());
		}
	}
	
	public static boolean hasBadValue(double[] values){
		for ( double value : values ){
			if ( !(value < 1 && value > -1) ){
				System.out.println(value + " is 'bad'");
				return true;
			}
		}
		return false;
	}
}
