import java.util.LinkedList;
import java.util.List;

import eu.fittest.tloglib.*;

public class TestOverloading {

	public int x ;
	   
    // a log-relevant constructor
	public TestOverloading() { 
		x=99;
		XLog.log("constructor0",x) ; 
	}

	 // a log-relevant constructor
	public TestOverloading(int x) { 
		this.x = x ;
		XLog.log("constructor1",x) ; 
	}

	// not log-relevant constructor
	public TestOverloading(int x, int y) { 
		this.x = x+y ;
	}
	
	public int foo() {
		int r = x ;
		XLog.log("foo0, returning ",r) ; 
		return r ;
	}
	
	public int foo(int x) {
		int r = x + this.x ;
		XLog.log("foo1, returning ",r) ; 
		return r ;
	}
	
	public int foo(int x, int y) {
		int r = x + this.x + y ;
		return r ;
	}
	
    public static void main(String args[]) {
	   XLog.initializeLogger() ;
	   new TestOverloading() ;
	   new TestOverloading(10) ;
	   new TestOverloading(10,11) ;
	   new TestOverloading(10).foo();
	   new TestOverloading(10).foo(10);
	   new TestOverloading(10).foo(10,11);
	   XLog.closeLogger() ;
	   TLog.printDebug() ;
    }

}
