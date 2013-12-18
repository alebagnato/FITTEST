import eu.fittest.tloglib.*;

public class TestWhile {
	
	int f(int x) {
		XLog.log("start f") ;
		int j=0 ;
		while (j<x) {
			if (j==10) XLog.log("j=",j) ;
			j++ ;
		}
		XLog.log("end f") ;
		return j ;
	}
	
	int g(int x) {
		XLog.log("start g") ;
		int j=0 ;
		if (j<0) return 0 ;
		else while (j<x) { 
			XLog.log("haha") ;
			j++ ;
		}
		XLog.log("end g") ;
		return j ;
	}
	
	int g2(int x) {
		XLog.log("start g2") ;
		int j=0 ;
		if (j<0) return 0 ;
		else while (j+f(0)<x) { 
			XLog.log("haha") ;
			j++ ;
		}
		XLog.log("end g2") ;
		return j ;
	}
	
	public static void main(String args[]) {
		   XLog.initializeLogger() ;
		   new TestWhile().f(0) ;
		   new TestWhile().f(12) ;
		   new TestWhile().g(9) ;
		   new TestWhile().g2(9) ;
		   XLog.closeLogger() ;
		   TLog.printDebug() ;
	}

}
