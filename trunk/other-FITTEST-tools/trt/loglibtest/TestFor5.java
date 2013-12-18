import java.util.LinkedList;
import java.util.List;

import eu.fittest.tloglib.* ;

public class TestFor5 {
	
	// A log-relevant nested loop. Note that it calls a log-relevant f,
	// which actually won't write any log. It throws exceptions inside
	// the inner loop. Since the whole for does not actually do any
	// logging, no bits should be spammed, and no exceptions should be
	// logged either.
	void loop() {
		TLog.log("start loop") ;
		long id1 = TLog.getLoopId() ;
		for(int i=0; TLog.ipush(id1,i<10) ; i++) {
			long id2 = TLog.getLoopId() ;
			for (int j=0; TLog.ipush(id2,j<3) ; j++) {
				TLog.tick() ;
				try { if (j==1) throw new Exception() ; 
				      f(0) ;
				      TLog.tick() ;
				}
				catch(Exception e){ 
					TLog.logE(e) ;
					TLog.tick() ;
				} 
			}
		}
		TLog.log("done") ;
		TLog.tick() ;
	}
	
	void f(int x) {
		if (TLog.push(x<0)) TLog.log("x is negative, ", x) ;
		TLog.tick() ;
	}
	
	static void loopDEC() throws Exception {
		DLog.log("start loop",10) ;
		for(int i__=0; true ; i__ ++) {
			if (i__ > 0) { } // encoding of i++  --> skip
			// encoding of i<x  --> skip
			if (! DLog.pop(11,29)) break ;
			
			for(int j__=0; true ; j__ ++) {
				if (j__ > 0) { } // encoding of i++  --> skip
				// encoding of i<x  --> skip
				if (! DLog.pop(15,20)) break ;
				
				DLog.tick() ;
				try { fDEC() ;
				      DLog.tick() ;
				}
				catch(Exception e){ 
					DLog.tick() ;
				} 
				
				
			}
		}
		DLog.log("done",30) ;
		DLog.tick() ;
	}
	
	static void fDEC() throws Exception {
		if (DLog.pop(17,19)) DLog.log("x is negative, ",19) ;
		DLog.tick() ;
	}

	
	public static void main(String[] args) throws Exception {
		TestFor5 z = new TestFor5() ;
		System.out.println("** " + z.getClass().getName()) ;
		TLog.initializeLogger() ;
		z.loop() ;
		TLog.closeLogger() ;
		
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		loopDEC() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}

}
