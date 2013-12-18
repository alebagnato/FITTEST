import eu.fittest.tloglib.* ;

import java.io.*;

public class TestException1 {
	
	public int f(int x) {
		XLog.log("begin f") ;
		try {
			if (x==0) throw new Exception() ;
			if (x==1) throw new IOException() ;
		}
		catch (IOException e) { 
			XLog.log("IOException handler block, x=",x) ;
		}
		catch (Exception e) { 
			XLog.log("Exception handler block, x=",x) ;
		}
		finally {
			XLog.log("finally block, x=",x) ;
		}
		XLog.log("end f") ;
		return x ;
	}

	public static void main(  String args[]){
		XLog.initializeLogger();
		new TestException1().f(0);
	    new TestException1().f(1);
	    new TestException1().f(2);
	    XLog.closeLogger() ;
		TLog.printDebug();
	 }

}
