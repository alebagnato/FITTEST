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

package com.stromberglabs.cluster.checker;

import com.stromberglabs.cluster.Cluster;
import com.stromberglabs.cluster.ClusterUtils;


/**
 * This cluster checker considers clustering done if all of the clusters moved
 * less than a certain fixed amount. 
 * 
 * @author Andrew
 *
 */
public class DriftClusterChecker implements ClusterChecker {
	
	private double mDriftTolerance;
	
	public DriftClusterChecker(double driftTolerance) {
		mDriftTolerance = driftTolerance;
	}
	
	public boolean recalculateClusters(Cluster[] clusters) {
		for ( Cluster cluster : clusters ){
			if ( cluster.getItems().size() > 0 ){
				double distanceChange = ClusterUtils.getEuclideanDistance(cluster.getClusterMean(),cluster.getLocation());
				if ( distanceChange > mDriftTolerance ){
					return true;
				}
			}
		}
		return false;
	}
}
