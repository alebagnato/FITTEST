import eu.fittest.tloglib.* ;
import eu.fittest.tloglib.event.*;

public class TestSeq {

	void f(int x) {
		TLog.log("begin of f, x=",x) ;
		x++ ;
		TLog.log("x=",x) ; x++ ;
		TLog.log("end of f, x=",x) ;
	}
	
	static void fDEC() throws Exception {
		//EventList z = DLog.eventlist ;
		DLog.log("begin of f, x=",5) ;
		DLog.log("x=",7) ;
		DLog.log("end of f, x=",8) ;
	}
	
	
	public static void main(String[] args) throws Exception {
		System.out.println("** TestSeq") ; 
		TLog.initializeLogger() ; 
		// tagged prog:
		TestSeq z = new TestSeq() ;
	   	z.f(9) ;

	   	DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
        // decoder:
		fDEC() ;
		DLog.tick() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}
}
