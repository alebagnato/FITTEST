import eu.fittest.tloglib.*;

public class TestLoopBreakContRet {
	
	// with break cont return
	public void f(int x) {
		XLog.log("f starts") ;
		int i=0 ;
		do {
			if (x==3 && i==0) { XLog.log("about to do continue, i=", i) ;
			   i +=2 ;
			   continue ;
			}
			if (x==5 && i+1==4) { XLog.log("breaking the loop, i=",i) ; break ; }
			if (x==10 && i+1==9){ XLog.log("return!, i=",i) ;return ; }
			i++ ;
		} while (i<x) ;
		XLog.log("f ends") ;
	}
	
	// loop that in itself does not log, but has a return
	public void g(int x) {
		XLog.log("g starts") ;
		int i=0 ;
		do {
			if (x==3 && i==0) {
			   i +=2 ;
			   continue ;
			}
			if (x==5 && i+1==4) { break ; }
			if (x==10 && i+1==9){ return ; }
			i++ ;
		} while (i<x) ;
		XLog.log("g ends") ;
	}
	
	// a loop in a log-relevant method; the loop contains break and cont, but 
	// does not contain return
	public void h(int x) {
		XLog.log("h starts") ;
		int i=0 ;
		do {
			if (x==3 && i==0) {
			   i +=2 ;
			   continue ;
			}
			if (x==5 && i+1==4) { break ; }
			i++ ;
		} while (i<x) ;
		XLog.log("h ends") ;
	}
	
	// loop with break/cont/ret in a method that is not log-relevant
	public void i(int x) {
		int i=0 ;
		do {
			if (x==3 && i==0) {
			   i +=2 ;
			   continue ;
			}
			if (x==5 && i+1==4) { break ; }
			if (x==10 && i+1==9){ return ; }
			i++ ;
		} while (i<x) ;
	}
	
	public static void main(String[] args) throws Exception {
		System.out.println("** TestLoopBreakContRet") ; 
		XLog.initializeLogger() ;

	    TestLoopBreakContRet z = new TestLoopBreakContRet() ;
		z.f(3) ; z.f(5) ; z.f(10) ;
		z.g(3) ; z.g(5) ; z.g(10) ;
		z.h(3) ; z.h(5) ; z.h(10) ;
		z.i(3) ; z.i(5) ; z.i(10) ;
		
		XLog.closeLogger() ;
		TLog.printDebug() ;
 	}

}
