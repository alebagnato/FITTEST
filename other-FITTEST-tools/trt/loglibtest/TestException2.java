import eu.fittest.tloglib.* ;

import java.io.*;

// To test an exception thrown before a try-catch block.

public class TestException2 {

	public int f1(int x) throws Exception {
		TLog.log("begin f1") ;
		if (x==0) throw new Exception() ;  // exception before try
		TLog.tick() ;
		try {
			if (x==0) throw new Exception() ; // will actually never be thrown
			TLog.tick() ;
		}
		catch (Exception e) { 
			TLog.logE(e) ;
			TLog.log("Exception handler of f1") ;
			TLog.tick() ;
		}
		finally {
			TLog.enterFinally() ;
			TLog.log("finally block of f1") ;
			TLog.tick() ;
		}
		TLog.log("end f1") ;
		TLog.tick() ; // tick before return
		return x ;
	}
	
	public void f2(){
		TLog.log("begin f2") ;
		TLog.tick() ;
		try {
			f1(0) ;
			TLog.tick() ;
		}
		catch (Exception e) { 
			TLog.logE(e) ;
			TLog.log("Exception handler of f2") ;
			TLog.tick() ;
		}
		finally {
			TLog.enterFinally() ;
			TLog.log("finally block of f2") ;
			TLog.tick() ;
		}
		TLog.log("end f2") ;
		TLog.tick() ;
	}
	
	
	public static void f1DEC() throws Exception {
		DLog.log("begin f1",0) ;
		DLog.tick() ;
		try {
			DLog.tick() ;
		}
		catch (Exception e) { 
			DLog.log("Exception handler of f1",0) ;
			DLog.tick() ;
		}
		finally {
			DLog.enterFinally() ;
			DLog.log("finally block of f1",0) ;
			DLog.tick() ;
		}
		DLog.log("end f1",0) ;
		DLog.tick() ;
	}
	
	public static void f2DEC() throws Exception  {
		DLog.log("begin f2",0) ;
		DLog.tick() ;
		try {
			f1DEC() ;
			DLog.tick() ;
		}
		catch (Exception e) { 
			DLog.log("Exception handler of f2",0) ;
			DLog.tick() ;
		}
		finally {
			DLog.enterFinally() ;
			DLog.log("finally block of f2",0) ;
			DLog.tick() ;
		}
		DLog.log("end f2",0) ;
		DLog.tick() ;
	}
	
	public static void main(String[] args) throws Exception {
		System.out.println("** TestException2") ; 
		TestException2 z = new TestException2() ;
		TLog.initializeLogger() ;
   		z.f2() ;
   		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		f2DEC() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}

}
