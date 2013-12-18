import eu.fittest.tloglib.*;

public class TestIfElse {

	void f(int x) {
		if (x>0)  XLog.log("x=",x) ;
		XLog.log("done") ;
	}

	void g(int x) {
		if (x>0)  {
			if (x>0) XLog.log("x=",x) ;
		}
		else {
			if (x<=0) x++ ;
		}
		XLog.log("done") ;
	}
	
	void h(int x) {
		if (x>0)  x++ ; else x = x+2 ;
		XLog.log("done") ;
	}
	
	public static void main(String args[]) {
		   XLog.initializeLogger() ;
		   new TestIfElse().f(-1) ;
		   new TestIfElse().f(9) ;
		   new TestIfElse().g(-1) ;
		   new TestIfElse().g(9) ;	
		   new TestIfElse().h(-1) ;
		   new TestIfElse().h(9) ;
		   XLog.closeLogger() ;
		   TLog.printDebug() ;
	}
	
}
