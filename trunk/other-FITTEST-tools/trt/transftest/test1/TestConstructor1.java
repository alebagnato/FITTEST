import eu.fittest.tloglib.*;

import java.util.*;

public class TestConstructor1 {
	
	//static class XLog {
	//	static void log(String s) { } ;
	//}
	
	public int x ;
	   
	    // a log-relevant constructor
	public TestConstructor1() { 
		x=99;
		XLog.log("creating a default Hello",x) ; 
		System.out.println("creating a default Hello") ;
		List<Object> s = new LinkedList<Object>() ;
	}
	   
	   // non-logging constructor
	public TestConstructor1(int x) { this.x = x ; }

    public static void main(String args[]) {
	   XLog.initializeLogger() ;
	   new TestConstructor1(0) ;
	   new TestConstructor1() ;
	   XLog.closeLogger() ;
	   TLog.printDebug() ;
    }

}
