import eu.fittest.tloglib.* ;

import java.io.*;

// throw exception to entry finally, the finally prematurely return
public class TestException5 {
	
	public void f1(int x) throws Exception {
		TLog.log("begin f1") ;
		TLog.tick() ;
		try {
			TLog.log("in the try-section of f1, x=",x) ;
			if (x==90) throw new Exception() ;
			x++ ;
			TLog.tick() ;
		}
        finally {
			TLog.enterFinally() ;
			TLog.log("finally block of f1, x=",x) ;
			TLog.tick() ;  
			if (1==1) return ;
			TLog.tick() ;  
		}
		TLog.log("SHOULD NOT be printed!") ;
		TLog.tick() ; // tick before return
		return  ;
	}
	
	public void f1DEC() throws Exception {
		DLog.log("begin f1",0) ;
		DLog.tick() ;
		try {
			DLog.log("in the try-section of f1, x=",0) ;
			DLog.tick() ;
		}
        finally {
			DLog.enterFinally() ;
			DLog.log("finally block of f1, x=",0) ;
			DLog.tick() ;  
			if (1==1) return ;
			DLog.tick() ;  
		}
		DLog.log("SHOULD NOT be printed!",0) ;
		DLog.tick() ; // tick before return
		return  ;
	}

	
	public static void main(String[] args) throws Exception {
		TestException5 z = new TestException5() ;
		System.out.println("** " + z.getClass().getName()) ; 
		TLog.initializeLogger() ;
   		z.f1(90) ;
   		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		z.f1DEC() ; DLog.tick() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}

}
