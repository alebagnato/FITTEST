import eu.fittest.tloglib.* ;

public class TestIf {
	
	void f(int x) {
		if (TLog.push(x>0))  TLog.log("x=",x) ;
		TLog.log("done") ;
	}

	static void fDEC() throws Exception {
		if (DLog.pop(6,7)) DLog.log("x=",6) ;
		DLog.log("done",7) ;
	}
	
	public static void main(String[] args) throws Exception {
		TestIf z = new TestIf() ;
		TLog.initializeLogger() ;	
		z.f(99) ; z.f(-7) ;
		
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		
		TestIf.fDEC() ; TestIf.fDEC() ;
		DLog.tick(); 
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}
}
