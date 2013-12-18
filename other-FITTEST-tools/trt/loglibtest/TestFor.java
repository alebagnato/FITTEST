import eu.fittest.tloglib.* ;

public class TestFor {
	
	public void f(int x) {
		TLog.log("start") ;
		long id1 = TLog.getLoopId() ;
		for (int i=0; TLog.ipush(id1, i<x); i++) {
			if (TLog.push(i==0)) TLog.log("first iteration, i=", i) ;
			if (TLog.push(i==x-1)) TLog.log("last iteration, i=",i) ;
		}
		TLog.log("end") ;
	}

	static public void fDEC() throws Exception {
		DLog.log("start",7) ;
		for (int k__ = 0; true; k__ ++) {
			if (k__ > 0) { } // encoding of i++  --> skip
			// encoding of i<x  --> skip
			if (! DLog.pop(8,12)) break ;
			if (DLog.pop(8,9)) DLog.log("first iteration, i=",8) ;
			if (DLog.pop(9,10)) DLog.log("last iteration, i=",9) ;
		}
		DLog.log("end",11) ;
	}
	
	public static void main(String[] args) throws Exception {
		System.out.println("** TestFor") ;
		TLog.initializeLogger() ;
		
		TestFor z = new TestFor() ;
		z.f(10) ; 
		
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		
		TestFor.fDEC() ; DLog.tick() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}
}
