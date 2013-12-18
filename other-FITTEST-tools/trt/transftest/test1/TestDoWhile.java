import eu.fittest.tloglib.*;

public class TestDoWhile {

	int f(int x) {
		int j=0 ;
		do {
			if (j==10) XLog.log("j=",j) ;
			j++ ;
		} 
		while (j<x) ;
		return j ;
	}
	
	 public static void main(  String args[]){
		XLog.initializeLogger();
		new TestDoWhile().f(0);
	    new TestDoWhile().f(12);
	    XLog.closeLogger() ;
		TLog.printDebug();
	 }
}
