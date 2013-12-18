import eu.fittest.tloglib.* ;

public class TestLoopBreakContRet {

	public void f(int x) {
		TLog.log("start") ;
		long id1 = TLog.getLoopId() ;
		int i=0 ;
		do {
			if (TLog.push(x==3 && i==0)) { TLog.log("about to do continue, i=", i) ;
			   i++ ;
			   TLog.tick() ;
			   continue ;
			}
			if (TLog.push(x==5 && i+1==4))  { TLog.log("breaking the loop, i=",i) ; TLog.tick() ; break ; }
			if (TLog.push(x==10 && i+1==9)) { TLog.log("return!, i=",i) ; TLog.tick() ; return ; }
			i++ ;
		} while (TLog.ipush(id1, i<x)) ;
		TLog.log("end") ;
	}
	
	static public void fDEC() throws Exception {
		DLog.log("start",7) ;
		for (int k__ = 0; true; k__ ++) {
			if (k__ > 0) {  
				// encoding of the guard i<x  --> skip
				if (! DLog.pop(9,18)) break ;
			}
			if (DLog.pop(10,11)) { 
				DLog.log("about to do continue, i=",10) ; DLog.tick() ; continue ;
			}
			if (DLog.pop(14,15)) { DLog.log("breaking the loop, i=",14) ; DLog.tick() ; break ; }
			if (DLog.pop(15,16)) { DLog.log("return!, i=",15) ; DLog.tick() ; return ; }
		}
		DLog.log("end",18) ;
	}
	
	public static void main(String[] args) throws Exception {
		System.out.println("** TestLoopBreakContRet") ; 
		TLog.initializeLogger() ;

		TestLoopBreakContRet z = new TestLoopBreakContRet() ;
		z.f(3) ; z.f(5) ; z.f(10) ;
		
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		fDEC() ; fDEC() ; fDEC() ;
		DLog.tick() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}
	
}
