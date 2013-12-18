import eu.fittest.tloglib.* ;

import java.io.*;

// test exception thrown inside a catch-section
public class TestException3 {
	
	public int f1(int x) throws Exception {
		TLog.log("begin f1") ;
		TLog.tick() ;
		try {
			if (x==90) throw new Exception() ;
			TLog.tick() ;
		}
		catch (Exception e) { 
			TLog.logE(e) ;
			x++ ;
			TLog.log("in handler of f1, x=",x) ;
			if (x==91) throw new Exception() ;  // exception inside catch
			x++ ;
			TLog.log("this should NOT be logged, x=",x) ;
			TLog.tick() ;
		}
		finally {
			TLog.enterFinally() ;
			TLog.log("finally block of f1, x=",x) ;
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
			f1(90) ;
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
			DLog.log("in handler of f1, x=",0) ;
			DLog.log("this should NOT be logged, x=",0) ;
			DLog.tick() ;
		}
		finally {
			DLog.enterFinally() ;
			DLog.log("finally block of f1, x=",0) ;
			DLog.tick() ;
		}
		DLog.log("end f1",0) ;
		DLog.tick() ; // tick before return
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
		System.out.println("** TestException3") ; 
		TestException3 z = new TestException3() ;
		TLog.initializeLogger() ;
   		z.f2() ;
   		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		f2DEC() ; DLog.tick() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}

}
