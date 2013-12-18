import eu.fittest.tloglib.* ;

import java.io.*;

// To test an exception thrown from inside a try-section.
public class TestException1 {
	
	public int exceptionInTry(int x) {
		TLog.log("begin f") ;
		TLog.tick() ;
		try {
			if (x==0) throw new Exception() ;
			if (x==1) throw new IOException() ;
			TLog.tick() ;
		}
		catch (IOException e) { 
			TLog.logE(e) ;
			TLog.log("IOException handler block, x=",x) ;
			TLog.tick() ;
		}
		catch (Exception e) { 
			TLog.logE(e) ;
			TLog.log("Exception handler block, x=",x) ;
			TLog.tick() ;
		}
		finally {
			TLog.enterFinally() ;
			TLog.log("finally block, x=",x) ;
			TLog.tick() ;
		}
		TLog.log("end f") ;
		TLog.tick() ; // tick before return too
		return x ;
	}
	
	//static void dummy(int x) throws Exception { if (x==0) throw new Exception() ; }

	static void exceptionInTryDEC() throws Exception {
		DLog.log("begin f",10) ;
		DLog.tick() ;
		try {
			DLog.tick() ;
		}
		catch(IOException e) {
			DLog.log("IOException handler block, x=",17) ;
			DLog.tick() ;
		}
		catch(Exception e){
			DLog.log("Exception handler block, x=",20) ;
			DLog.tick() ;
		}
		finally {
			DLog.enterFinally() ;
			DLog.log("finally block, x=",23) ;
			DLog.tick() ;
		}
		DLog.log("end f",14) ;
		DLog.tick() ;
	}
	
	public static void main(String[] args) throws Exception {
		System.out.println("** TestException1") ; 
		TestException1 z = new TestException1() ;
		TLog.initializeLogger() ;
   		z.exceptionInTry(0) ; // throw IOException
		z.exceptionInTry(1) ; // throw Exception
		z.exceptionInTry(2) ; // does not throw an exception
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		exceptionInTryDEC() ;
		exceptionInTryDEC() ;
		exceptionInTryDEC() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}

}
