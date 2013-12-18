import eu.fittest.tloglib.* ;

public class TestRecursion1 {
	
	public void top() {
		int r = fib(2) ;
		TLog.log("fib = ", r) ;
		r = fib(7) ;
		TLog.log("fib = ", r) ;
	}
	
	public int fib(int n) {
		if (TLog.push(n<=0)) { 
			//TLog.log("Base case 0, n=",n); 
			return 0 ; 
			}
		if (TLog.push(n==1)) { 
			return 1 ; 
			}	
		if (TLog.push(n==5)) { TLog.log("n is ",n) ; }
		int r1 = 0 ;
		int r2 = 0 ;
		// recursive call is translated to a dummy singleton loop
		long lid = TLog.getLoopId() ;
		TLog.getIterationLogBuffer().addIteration(lid) ;
		for(int i=0; TLog.ipush(lid,i<1); i++) {
			r1 = fib(n-1) ;
		}
		
		lid = TLog.getLoopId() ;
		for(int i=0; TLog.ipush(lid,i<1); i++) {
		    r2= fib(n-2) ;
		}

		return r1+r2 ;
	}

	static public void topDEC() throws Exception {
		fibDEC() ;
		DLog.log("fib = ", 3) ;
		fibDEC() ;
		DLog.log("fib = ", 5) ;
	}
	
	static public void fibDEC() throws Exception {
		if (DLog.pop(8,10))  { 
			//DLog.log("Base case 0, n=",9); 
			return ; 
		}
	    if (DLog.pop(10,12)) { return ; }
	    
	    if (DLog.pop(13,14)) { DLog.log("n is ",15) ; }
	    
	    
	    for(int i__=0; true ; i__ ++) {
			if (i__ > 0) { } // encoding of i++  --> skip
			// encoding of i<x  --> skip
			if (! DLog.pop(23,24)) break ;
			fibDEC() ;
	    }
	    
	    for(int i__=0; true ; i__ ++) {
			if (i__ > 0) { } // encoding of i++  --> skip
			// encoding of i<x  --> skip
			if (! DLog.pop(25,26)) break ;
			fibDEC() ;
	    }

	}
	
	public static void main(String[] args) throws Exception {
		System.out.println("** TestFor") ;
		TLog.initializeLogger() ;
		
		TestRecursion1 z = new TestRecursion1() ;
		z.top() ; 
		
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		
		TestRecursion1.topDEC() ; DLog.tick() ;
		
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}
}
