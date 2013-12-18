import eu.fittest.tloglib.* ;

public class TestDoWhile {

	public void f(int x) {
		TLog.log("start") ;
		long id1 = TLog.getLoopId() ;
		int i=0 ;
		do {
			if (TLog.push(i==0))   TLog.log("first iteration, i=", i) ;
			
			if (TLog.push(i+1==x)) TLog.log("last iteration, i=",i) ;
			i++ ;
		} while (TLog.ipush(id1, i<x)) ;
		//TLog.log("end") ;
	}
	
	static public void fDEC() throws Exception {
		DLog.log("start",6) ;
		boolean firstIteration = true ;
		while (true) {
			if (! firstIteration) {  
				// encoding of the guard i<x  --> skip
				if (! DLog.pop(7,14)) break ;
			}
			if (DLog.pop(8,9)) DLog.log("first iteration, i=",8) ;
			if (DLog.pop(10,11)) DLog.log("last iteration, i=",10) ;
			firstIteration = false ;
		}
		//DLog.log("end",14) ;
	}
	
	public static void main(String[] args) throws Exception {
		System.out.println("** TestDoWhile") ; 
		TLog.initializeLogger() ;
		
		TestDoWhile z = new TestDoWhile() ;		
		z.f(10) ;
		
		TLog.printDebug() ;
		
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		
		fDEC() ; 
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}
}
