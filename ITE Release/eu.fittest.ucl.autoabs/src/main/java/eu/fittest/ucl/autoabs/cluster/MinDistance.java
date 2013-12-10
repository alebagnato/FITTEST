/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */
package eu.fittest.ucl.autoabs.cluster;

import java.math.BigDecimal;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;

/**
 * A class to compute the minimum distance between two clusters. 
 */
public class MinDistance extends DistanceFunction {

	/**
	 * A concrete state represents a single CSV line, e.g. 
	 * EVENT,numOfSelectedItems,numInShopCart,cartTotal,numInCompareCart,catalogContents
	 * find_phones,18,0,0.00,0,""
	 * 
	 * To compute distance between two clusters, I iterate over all states in both clusters
	 * For each two states, I add the difference between all numeric values in the state (e.g. cartTotal, numInShoppingCart)
	 * and the hamming distance between all string values in the state
	 * 
	 * If the total distance between two states in a cluster is the smallest found thus far, I use it
	 * as the min distance
	 * 
	 * @param c1 A cluster containing a list of concrete states
	 * @param c2 A cluster containing a list of concrete states
	 * @return double The minimum distance between two concrete states in clusters c1 and c2
	 * */
	@Override
	public double computeDistance(Cluster c1, Cluster c2) {
		if(c1.getID() == c2.getID())
			throw new IllegalArgumentException("clusters are the same");
		double min = Double.MAX_VALUE;
		LinkedHashSet<ConcreteState> c1States = c1.getStates();
		LinkedHashSet<ConcreteState> c2States = c2.getStates();
		Iterator<ConcreteState> i1 = c1States.iterator();
		while(i1.hasNext()) {
			ConcreteState s1 = i1.next();
			List<BigDecimal> s1Doubles = s1.getDoubleValues();
			List<String> s1Strings = s1.getStringValues();
			
			Iterator<ConcreteState> i2 = c2States.iterator();
			while(i2.hasNext()) {
				ConcreteState s2 = i2.next();
				double diff = 0;
				diff += super.computeDoubleListDifference(s1Doubles, s2.getDoubleValues());
				diff += super.computeStringListDifference(s1Strings, s2.getStringValues());
				if(diff < min)
					min = diff;
			}
		}
		return min;
	}

}
