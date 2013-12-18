import eu.fittest.tloglib.* ;

public class TestSwitch {
	
	public int f(int x) {
		
		int result = 0 ;
		
		switch (x) {
		case 0  : result += 2 ; break ;
		default : result = 0  ;
		}	

		switch (x) {
		case 0 : XLog.log("Non cascading case, x=", x) ; break ; 
		case 1 : XLog.log("Cascading case, x=", x) ;
		case 2 : result++ ;
		case 10 : XLog.log("Ending cascade, result=", result) ; break ;
		default : XLog.log("Default, result=", result) ;
		}	
		return result ;
	}
	
	public int g(int x) {
		// switch with distinct-cases
		switch (x) {
		case 0  : XLog.log("case x=0") ; break ;
		case 2  : x=0 ; break ;
		default : ; //XLog.log("Default case, x=", x) ;
		}	
		return x ;
	}
	
	public static void main(String args[]) {
		   XLog.initializeLogger() ;
		   new TestSwitch().f(0) ;
		   new TestSwitch().f(1) ;
		   new TestSwitch().f(2) ;
		   new TestSwitch().f(3) ;
		   new TestSwitch().f(10) ;
		   XLog.closeLogger() ;
		   TLog.printDebug() ;
		}

}
