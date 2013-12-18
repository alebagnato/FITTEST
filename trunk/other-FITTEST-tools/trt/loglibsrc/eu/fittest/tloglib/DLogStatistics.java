package eu.fittest.tloglib;

import java.util.* ;

public class DLogStatistics {
	
	public int blogLength ;
	public int elogLength ;
	public int decodedLogLength ;
	public int orgLogLength ;
	public int orgNumOfLogCall ;
	
	public DLogStatistics(int blogLength, int elogLength, int decodedLogLength, int orgLogLength, int orgNumOfLogCall) 
	{
		this.blogLength = blogLength ;
		this.elogLength = elogLength ;
		this.decodedLogLength = decodedLogLength ;
		this.orgLogLength = orgLogLength ;
		this.orgNumOfLogCall = orgNumOfLogCall ;
	}
	
	public double enhancementFactor(){
		if (orgLogLength==0) return -1 ;
		return (double) decodedLogLength / (double) orgLogLength ;
	}
	
	public double compressionRate(){
		if (orgLogLength==0) return -1 ;
		return (double)(blogLength + elogLength) / (double) orgLogLength ;
	}
	
	public double optimisticCompressionRate(){
		if (orgLogLength==0) return -1 ;
		return (optimisticBlogLength() + (double) elogLength) 
				/ (double) orgLogLength ;
	}
	
	// the ration between the total size of logged dynamic values and the size of
	// the original log. The total size of dynamic values is approximated by the
	// size of event log.
	public double dynamicValRatio(){
		if (orgLogLength==0) return -1 ;
		return (double) elogLength / (double) orgLogLength ;
	}
	
	// optimistic length of the binary encoded log, in characters (which are two bytes)
	public double optimisticBlogLength() {
		return (double) blogLength/16.0 ;
	}
	
	public static DLogStatistics sum(List<DLogStatistics> stats){
		DLogStatistics r = new DLogStatistics(0,0,0,0,0) ;
		for (DLogStatistics s : stats) {
			r.blogLength += s.blogLength ;
			r.elogLength += s.elogLength ;
			r.decodedLogLength += s.decodedLogLength ;
			r.orgLogLength += s.orgLogLength ;
			r.orgNumOfLogCall += s.orgNumOfLogCall ;
		}
		return r ;
	}
	
	public void printStatistics() {
		System.out.println("number of log-calls  = " + orgNumOfLogCall) ;
		System.out.println("number of bits       = " + blogLength) ;
		if (orgNumOfLogCall != 0)
			System.out.println("avrg bits overhead per log-call = " + blogLength/orgNumOfLogCall) ;
		else
			System.out.println("avrg bits overhead per log-call = infinite") ;
		System.out.println("optimistic bit-encoding trace-length (bytes)  = " + blogLength*2) ;
		System.out.println("pesimistic char-encoding trace-length (bytes) = " + optimisticBlogLength()*2) ;
		System.out.println("length of enhanced log (bytes) = " + decodedLogLength*2) ;
		System.out.println("length of original log (bytes) = " + orgLogLength*2) ;
		System.out.println("length of event log    (bytes) = " + elogLength*2) ;
		System.out.println("compression factor, optimistic = " 
		                        + optimisticCompressionRate() 
		                        + ", pesimistic = " + compressionRate()) ;
		System.out.println("enhancement factor = " + enhancementFactor()) ;
		System.out.println("dyn. value ratio   = " + dynamicValRatio()) ;
	}
	
}
