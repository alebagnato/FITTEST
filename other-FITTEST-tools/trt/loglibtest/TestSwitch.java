import eu.fittest.tloglib.* ;

public class TestSwitch {
	
	public int f(int x) {
		int result = 0 ;
		
		switch(x) {
		case 0 : TLog.push("000") ; break ;
		case 1 : TLog.push("001") ; break ;
		case 2 : TLog.push("010") ; break ;
		case 10 : TLog.push("011"); break ;
		default : TLog.push("100") ;
		}
		
		switch (x) {
		case 0 : TLog.log("Non cascading case, x=", x) ; TLog.tick() ; break ;
		case 1 : TLog.log("Cascading case, x=", x) ;
		case 2 : result++ ;
		case 10 : TLog.log("Ending cascade, result=", result) ; TLog.tick() ; break ;
		default : TLog.log("Default, result=", result) ;
		}	
		TLog.log("end") ;
		TLog.tick() ;
		return result ;
	}
	
	static public void fDEC() throws Exception {
		int code = DLog.pop(3) ;
		switch (code) {
		case 0 : DLog.logLnNr(17) ; DLog.log("Non cascading case, x=", 17) ; DLog.tick() ; break ;
		case 1 : DLog.logLnNr(18) ; DLog.log("Cascading case, x=", 18) ;
		case 2 : DLog.logLnNr(19) ;
		case 3 : DLog.logLnNr(20) ; DLog.log("Ending cascade, result=", 20) ;  DLog.tick() ; break ;
		default : DLog.logLnNr(21) ; DLog.log("Default, result=", 21) ;
		}
		DLog.tick() ;
		DLog.log("end",23) ;
	}
	
	public static void main(String[] args) throws Exception {
		System.out.println("** TestSwitch") ; 
		TLog.initializeLogger() ;
		
		TestSwitch z = new TestSwitch() ;
		z.f(0) ; 
		z.f(1) ; 
		z.f(2) ; 
		z.f(10) ; 
		z.f(5) ;
		
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		fDEC() ; 
		fDEC() ; 
		fDEC() ;  
		fDEC() ; 
		fDEC() ;
		DLog.tick() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}

}
