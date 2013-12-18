import eu.fittest.tloglib.*;

public class TestIf {
	
	void f(int x) {
		if (x>0)  
			XLog.log("x=",x) ;
		XLog.log("done") ;
	}

	public static void main(String args[]) {
		   XLog.initializeLogger() ;
		   new TestIf().f(-1) ;
		   new TestIf().f(9) ;
		   XLog.closeLogger() ;
		   TLog.printDebug() ;
	}
}
