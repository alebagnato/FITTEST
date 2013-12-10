/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */

package eu.fittest.ucl.autoabs.cluster;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import eu.fittest.ucl.autoabs.utils.Utils;
import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;

/**
 * Hierarchical clustering algorithm, constructing a dendrogram
 *
 */
public class ClusterAlgo {
	
	/** the dendrogram containing all the possible clusterings for a given distance function */
	private Map<Integer, LinkedHashSet<Cluster>> dendogram;
	
	/** the max K value of a dendrogram */
	private int maxK;
	
	/** distance function used to decide which clusters to merge */
	private final DistanceFunction distanceFunction;
	
	public ClusterAlgo(Map<Integer, LinkedHashSet<Cluster>> clustering, int maxK) {
		this.dendogram = clustering;
		this.maxK = maxK;
		this.distanceFunction = new MinDistance();
	}
	
	public ClusterAlgo(DistanceFunction distance)
	{
		this.dendogram = new HashMap<Integer, LinkedHashSet<Cluster>>();
		this.distanceFunction = distance;
	}
	
	/**
	 * This function computes all the distances between each cluster for a given 'k' and
	 * saves the distances in this.distance list. It remembers the two clusters with the
	 * smallest distance
	 */
	private Cluster[] computeClusterDistances(LinkedHashSet<Cluster> clusters)
	{
		int minDistanceIndex = -1;
		int n = clusters.size();
		double[] distances = new double[((n - 1)*n)/2];
		Set<Integer> compared = new HashSet<Integer>();
		int index = 0;
		//this.clusters.size();
		Cluster[] closest = new Cluster[2];
		closest[0] = closest[1] = null;
		Iterator<Cluster> it = clusters.iterator();
		while(it.hasNext()) {
			Cluster c1 = it.next();
			Iterator<Cluster> it2 = clusters.iterator();
			while(it2.hasNext()) {
				Cluster c2 = it2.next();
				if(c1.getID() == c2.getID())
					continue;
				int _id = c1.getID() | c2.getID();
				if(compared.contains(_id)) continue;
				compared.add(_id);
				double distance = this.distanceFunction.computeDistance(c1, c2);
				distances[index] = distance;
				if(minDistanceIndex == -1 || (distances[minDistanceIndex] > distance))
				{
					minDistanceIndex = index;
					closest[0] = c1;
					closest[1] = c2;
				}
				
				index++;
			}
		}
		compared.clear();
		compared = null;
		distances = null;
		return closest;
	}
	
	/** This function returns the two clusters that are closest in distance term
	 * @param clusters The cluster collection
	 * @return Two closest clusters
	 */
	public Cluster[] getClosestClusters(Collection<Cluster> clusters) {
		return this.computeClusterDistances(new LinkedHashSet<Cluster>(clusters));
	}
	
	/**
	 * This function adds the cluster c to the dendrogram at a given 'k'
	 * @param c Cluster to add to the dendrogram
	 * @param k The level at which to add the cluster
	 */
	private void updateDendogram(Cluster c, int k)
	{
		if(this.dendogram.containsKey(k))
		{
			this.dendogram.get(k).add(c);
		}
		else
		{
			LinkedHashSet<Cluster> clusters = new LinkedHashSet<Cluster>();
			clusters.add(c);
			this.dendogram.put(k, clusters);
		}
	}
	
	/**
	 * This function constructs the dendrogram from a list of states. 
	 * @param states A vector of concrete state lists (i.e. each entry in the list is a rows in the original CSV file)
	 */
	public void agglomerate(LinkedHashSet<ConcreteState> states)
	{		
		int n = states.size();
		int k = n;
		this.maxK = k;
		LinkedHashSet<Cluster> clusters = new LinkedHashSet<Cluster>(n);
		
		Cluster[] closest = null;
		
		Iterator<ConcreteState> it = states.iterator();
		while(it.hasNext()) {
			Cluster c = new Cluster(it.next(), null);
			clusters.add(c);
			this.updateDendogram(c, k);
		}
		
		closest = this.computeClusterDistances(clusters);
			
		while(clusters.size() > 1) {
			
			k--;

			Cluster m = new Cluster(closest[0], closest[1]);
			
			closest[0].setParent(m);
			closest[1].setParent(m);
			
			clusters.remove(closest[0]);
			clusters.remove(closest[1]);
			
			clusters.add(m);

			this.dendogram.put(k, new LinkedHashSet<Cluster>(clusters));
			
			closest = this.computeClusterDistances(clusters);
		}
		//k == 1
		LinkedHashSet<Cluster> single = new LinkedHashSet<Cluster>();
		single.add(clusters.toArray(new Cluster[1])[0]);
		this.dendogram.put(1, single);
		clusters.clear();
		clusters = null;
		closest = null;
	}
	
	public int getMaxK()
	{
		return this.maxK;
	}
	
	/** FOR TESTING ONLY
	 * @param clusterId The id of the cluster to return
	 * @return A cluster (if found) or null
	 */
	public Cluster getCluster(int clusterId) {
		Collection<LinkedHashSet<Cluster>> allClusters = this.dendogram.values();
		for(LinkedHashSet<Cluster> clusters : allClusters)
		{
			for(Cluster c : clusters)
				if(c.getID() == clusterId)
					return c;
		}
		return null;
	}
	
	/**
	 * Returns the cluster id that contains a state at a given level 'k'
	 * Assumes 1 <= k <= maxK
	 * @param state A ConcreteState whose cluster we want to get
	 * @param k The level at which to look for the cluster
	 * @return A cluster id
	 * @throws Exception 
	 */
	public int getClusterId(ConcreteState state, int k)
	{
		LinkedHashSet<Cluster> clusters = this.dendogram.get(k);
		for(Cluster c : clusters)
		{
			if(c.containsState(state))
				return c.getID();
		}
		
		throw new RuntimeException("cluster not found: "  + state.toString());
	}
	
	/** FOR TESTING ONLY
	 * Return all cluster ids for a given level
	 * @param k The level whose clusters to return
	 * @return An array of cluster ids
	 */
	public int[] getClusterIDs(int k)
	{
		LinkedHashSet<Cluster> clusters = this.dendogram.get(k);
		int[] ids = new int[clusters.size()];
		Iterator<Cluster> it = clusters.iterator();
		int i = 0;
		while(it.hasNext()) {
			ids[i++] = it.next().getID();
		}
		return ids;
	}
	
	/**
	 * Function to print the dendrogram
	 */
	@Override
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append("digraph dendrogram {" + Utils.eol);
		for(Entry<Integer, LinkedHashSet<Cluster>> entry : this.dendogram.entrySet()) {
			int k = entry.getKey();
			LinkedHashSet<Cluster> clusters = entry.getValue();
			
			StringBuilder kBuilder = new StringBuilder();
			kBuilder.append("k = " + String.valueOf(k) + " { ");
			boolean appendComma = false;
			for(Cluster cluster : clusters) {
				if(appendComma)
					kBuilder.append(",");
				kBuilder.append(String.valueOf(cluster.getID()));
				appendComma = true;
			}
			kBuilder.append(" }");
			
			sb.append("subgraph cluster_" + String.valueOf(k) + " {" + Utils.eol);
			sb.append("style=filled;" + Utils.eol + "color=lightgrey;" + Utils.eol + "label=\"" + kBuilder.toString() + "\";" + Utils.eol);
			sb.append("node [style=filled,color=white];" + Utils.eol);
			
			if(k == 1) {
				Cluster cluster = clusters.toArray(new Cluster[1])[0];
				sb.append(String.valueOf(cluster.getID()) + " [label=\"id:"+ String.valueOf(cluster.getID()) + ", " + cluster.toString()+"\"];" + Utils.eol);
			} else if(k == this.maxK) {
				for(Cluster cluster : clusters) {
					sb.append(String.valueOf(cluster.getID()) + " [label=\"id:"+ String.valueOf(cluster.getID()) + ", " + cluster.toString()+"\"];" + Utils.eol);
					if(cluster.getParent() != null){
						sb.append(String.valueOf(cluster.getID()) + " -> " + String.valueOf(cluster.getParent().getID()) + ";" + Utils.eol);
					}
				}
			} else {
				Cluster merged = clusters.toArray(new Cluster[clusters.size()])[clusters.size() - 1];
				sb.append(String.valueOf(merged.getID()) + " [label=\"id:"+ String.valueOf(merged.getID()) + ", " + merged.toString()+"\"];" + Utils.eol);
				if(merged.getParent() != null){
					sb.append(String.valueOf(merged.getID()) + " -> " + String.valueOf(merged.getParent().getID()) + ";" + Utils.eol);
				}
			}
			
			sb.append("}" + Utils.eol);
		}
		
		sb.append("}" + Utils.eol);
		return sb.toString();
	}
	
	/** Getter method for dendrogram */
	public Map<Integer, LinkedHashSet<Cluster>> getDendrogram(){
		return this.dendogram;
	}
	
	/** Empty the denogram because I only need it to initialize 
	 * chromosomes. after that, chromsomes generate their own clusters
	 */
	public void cleanup() {
		this.dendogram.clear();
		this.dendogram = null;
	}
}
