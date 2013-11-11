/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */
package eu.fittest.ucl.autoabs.cluster;

import java.math.BigDecimal;
import java.util.List;

/** 
 * Abstract class for computing distances between two clusters. 
 *
 */
public abstract class DistanceFunction {
	public abstract double computeDistance(Cluster c1, Cluster c2);
	
	/**
	 * This function computes the total difference between two lists of Doubles
	 * This function requires that the two lists are of equal size
	 * @param l1 List of Double values
	 * @param l2 List of Double values
	 * @return The sum of the differences between each pair of double values
	 */
	public static double computeDoubleListDifference(List<BigDecimal> l1, List<BigDecimal> l2){
		BigDecimal diff = BigDecimal.ZERO;
		assert(l1.size() == l2.size());
		for(int i = 0; i < l1.size(); i++){
			diff = diff.add((l1.get(i).subtract(l2.get(i))).abs());
			//diff += Math.abs(l1.get(i) - l2.get(i));
		}
		return diff.doubleValue();
	}
	
	/**
	 * This function computes the sum of hamming distances between two lists of Strings
	 * This function requires that the two lists are of equal size
	 * @param l1 List of String values
	 * @param l2 List of String values
	 * @return The sum of the hamming distances between each pair of string items
	 */
	public static int computeStringListDifference(List<String> l1, List<String> l2){
		int diff = 0;
		assert(l1.size() == l2.size());
		for(int i = 0; i < l1.size(); i++){
			diff += computeHammingDistance(l1.get(i), l2.get(i));
		}
		return diff;
	}
	
	/**
	 * This function computes the hamming distance between two strings
	 * This function requires that both s1 and s2 are not null
	 * @param s1 An arbitrary string
	 * @param s2 An arbitrary string
	 * @return The hamming distance between s1 and s2
	 */
	private static int computeHammingDistance(String s1, String s2){
		if(s1.equals(s2)) return 0;
		
		int d = Math.abs(s1.length() - s2.length());
		int limit = s1.length() < s2.length()? s1.length():s2.length();
		
		for(int i = 0; i < limit; i++){
			char c1 = s1.charAt(i);
			char c2 = s2.charAt(i);
			if(c1 != c2) d++;
		}
		
		return d;
	}
}
