import eu.fittest.tloglib.*;

public class TestSeq {

	void f(int x) {
		XLog.log("begin of f") ;
		x++ ;
		XLog.log("x=",x) ;
		XLog.log("end of f") ;
	}
	
	public static void main(String[] args) {
		XLog.initializeLogger() ;
		new TestSeq().f(0) ;
		XLog.closeLogger() ;
		TLog.printDebug() ;
	}
	
}
