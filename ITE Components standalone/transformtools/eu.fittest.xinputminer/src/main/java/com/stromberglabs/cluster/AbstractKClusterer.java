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

import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import com.stromberglabs.cluster.checker.ClusterChecker;
import com.stromberglabs.cluster.checker.DriftClusterChecker;
import com.stromberglabs.cluster.Clusterable;

/**
 * Abstract K Means Clusters, provides the command loop:
 * 
 * 1) Calculate Initial Clusters
 * 2) Assign all the values to their closest cluster
 * 3) Check to see if it needs to be recalculated
 *    - If yes, calculate new clusters and go to 2)
 *    - If no, return current set of clusters  
 * 
 * @author Andrew
 *
 */
public abstract class AbstractKClusterer implements KClusterer {
	public static double DISTANCE_TOLERANCE = 0.005;
	public static int MAX_RECLUSTERING = 100;
	
	int mMaxReclustering = MAX_RECLUSTERING;
	ClusterChecker mChecker;
	
	protected AbstractKClusterer(){
		this(new DriftClusterChecker(DISTANCE_TOLERANCE),MAX_RECLUSTERING);
	}
	
	protected AbstractKClusterer(ClusterChecker checker){
		this(checker,MAX_RECLUSTERING);
	}
	
	protected AbstractKClusterer(ClusterChecker checker, int maxReclustering){
		mChecker = checker;
		mMaxReclustering = maxReclustering;
	}
	
	public Cluster[] cluster(final List<? extends Clusterable> values, int numClusters) {
		Cluster[] clusters = calculateInitialClusters(values,numClusters);
		
		boolean recalculateClusters = true;

		int numIterations = 0;
		while ( recalculateClusters ){
			//add all items to nearest cluster
			clusters = assignClusters(clusters,values);
			
			//see if the cluster distance hasn't moved
			recalculateClusters = mChecker.recalculateClusters(clusters);
			
			//if it needs to be run again, set up new clusters on the updated centers
			if ( recalculateClusters ){
				if ( numIterations > mMaxReclustering ){
					recalculateClusters = false;
				}
				
				clusters = getNewClusters(clusters);
				
				numIterations++;
			}
		}
		
		return clusters;
	}

	protected abstract Cluster[] assignClusters(Cluster[] clusters,final List<? extends Clusterable> values);
	
	protected abstract Cluster[] getNewClusters(Cluster[] clusters);
	
	/**
	* Calculates the initial clusters randomly, this could be replaced with a better algorithm
	* @param values
	* @param numClusters
	* @return
	*/
	protected Cluster[] calculateInitialClusters(List<? extends Clusterable> values, int numClusters){
		Cluster[] clusters = new Cluster[numClusters];
		//choose centers and create the initial clusters
		Random random = new Random(1);
		Set<Integer> clusterCenters = new HashSet<Integer>();
		for ( int i = 0; i < numClusters; i++ ){
			int index = random.nextInt(values.size());
			while ( clusterCenters.contains(index) ){
				index = random.nextInt(values.size());
			}
			clusterCenters.add(index);
			clusters[i] = new Cluster(values.get(index).getLocation(),i);
		}
		return clusters;
	}
}
