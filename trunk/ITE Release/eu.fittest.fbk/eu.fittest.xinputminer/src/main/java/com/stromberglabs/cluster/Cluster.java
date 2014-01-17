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

import java.util.LinkedList;
import java.util.List;

import com.stromberglabs.cluster.Clusterable;

public class Cluster implements Clusterable {
	private float[] mOriginalMeanLocation;
	private float[] mCurrentMeanLocation;
	private List<Clusterable> mClusterItems;
	
	private int id;
	
	public Cluster(float[] location, int id){
		mOriginalMeanLocation = location;
		mClusterItems = new LinkedList<Clusterable>();
		this.id = id;
	}
	
	/**
	 * Get the current mean value of the cluster's items
	 * @return
	 */
	public float[] getClusterMean(){
		float[] normedCurrentLocation = new float[mCurrentMeanLocation.length];
		for ( int i = 0; i < mCurrentMeanLocation.length; i++ ){
			normedCurrentLocation[i] = mCurrentMeanLocation[i]/((float)mClusterItems.size());
		}
		return normedCurrentLocation;
	}
	
	public void removeItem(Clusterable item){
		mClusterItems.remove(item);
	}
	
	public void addItem(Clusterable item){
		if ( mCurrentMeanLocation == null ){
			mCurrentMeanLocation = item.getLocation().clone();
		} else {
			mCurrentMeanLocation = sumArrays(mCurrentMeanLocation, item.getLocation());
		}
		mClusterItems.add(item);
	}
	
	public List<Clusterable> getItems(){
		return mClusterItems;
	}
	
	/**
	 * Get the original location of the cluster
	 */
	public float[] getLocation() {
		return mOriginalMeanLocation;
	}
	
	public void setLocation(float[] location){
		mOriginalMeanLocation = location;
	}
	
	public static float[] getMeanValue(List<Clusterable> items){
		assert(items != null);
		assert(items.size() > 0);
		float[] newLocation = new float[items.get(0).getLocation().length];
		for ( Clusterable item : items ){
			newLocation = sumArrays(newLocation, item.getLocation());
		}
		for ( int i = 0; i < newLocation.length; i++ ){
			newLocation[i] = newLocation[i]/items.size();
		}
		return newLocation;
	}
	
	private static float[] sumArrays(float[] valsA, float[] valsB){
		for ( int i = 0; i < valsA.length; i++ ){
			valsA[i] += valsB[i];
		}
		return valsA;
	}
	
	public static void main(String args[]){
		float center[] = {0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F};
		Cluster c = new Cluster(center,0);
		float center2[] = {1F,1F,1F,1F,1F,1F,1F,1F,1F,1F,1F,1F,1F,1F,1F,1F,1F,1F,1F,1F};
		c.addItem(new Cluster(center2,0));
		System.out.print("avg = [");
		for ( double val : c.getClusterMean() ){
			System.out.print(val + ",");
		}
		System.out.println("]");
		float center3[] = {0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F,0F};
		c.addItem(new Cluster(center3,0));
		System.out.print("avg = [");
		for ( double val : c.getClusterMean() ){
			System.out.print(val + ",");
		}
		System.out.println("]");
		float center4[] = {-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F,-4F};
		c.addItem(new Cluster(center4,0));
		System.out.print("avg = [");
		for ( double val : c.getClusterMean() ){
			System.out.print(val + ",");
		}
		System.out.println("]");
	}
	
	public String toString(){
		return String.valueOf(id);
	}
	
	public int getId(){
		return id;
	}
}
