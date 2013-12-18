import eu.fittest.tloglib.* ;

import java.io.*;

// test return in the middle, with a finally section
public class TestException4 {
	
	public void f1(int x) throws Exception {
		TLog.log("begin f1") ;
		TLog.tick() ;
		try {
			if (x==90) throw new Exception() ;
			TLog.tick() ;
		}
		catch (Exception e) { 
			TLog.logE(e) ;
			TLog.log("in handler of f1, x=",x) ;
			x++ ;
			TLog.tick() ; // tick before return
			return ;
			// TLog.tick() ; should be there, but rejected by javac as deadcode
		}
		finally {
			TLog.enterFinally() ;
			TLog.log("finally block of f1, x=",x) ;
			TLog.tick() ;
		}
		TLog.log("end f1") ;
		TLog.tick() ; // tick before return
		return  ;
	}
	
	
	public void f1DEC() throws Exception {
		DLog.log("begin f1",0) ;
		DLog.tick() ;
		try {
			DLog.tick() ;
		}
		catch (Exception e) { 
			DLog.log("in handler of f1, x=",0) ;
			DLog.tick() ; // tick before return
			return ;
			// TLog.tick() ; should be there, but rejected by javac as deadcode
		}
		finally {
			DLog.enterFinally() ;
			DLog.log("finally block of f1, x=",0) ;
			DLog.tick() ;
		}
		DLog.log("end f1",0) ;
		DLog.tick() ; // tick before return
		return  ;
	}
	
	public static void main(String[] args) throws Exception {
		TestException4 z = new TestException4() ;
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
