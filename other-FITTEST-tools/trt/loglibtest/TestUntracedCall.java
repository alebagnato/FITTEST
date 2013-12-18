import eu.fittest.tloglib.* ;
import eu.fittest.tloglib.event.UntracedCall;

public class TestUntracedCall {
    int x = 0 ;
    
    void f0() { 
    	TLog.untracedCall("!TestUntracedCall$DECoder!f0DEC") ;
    	f0_T() ; // calling the actual method
    }
	void f0_T() { TLog.log("executing f0, x=",x) ; }
	
	// simulating an external module:
	void fexternal() {
		// call back to logging methods:
		f0() ; f0() ;
	}
	
	void f() {
		x=95 ;
		TLog.log("entering f, x=",x) ;
		f0_T() ; // normal call to f0
		fexternal() ; // call to an external module
		TLog.log("end of f, x=",x) ;
	}
	
	// the decoder-class
	static public class DECoder {
		
		static public void fDEC() throws Exception {
			//System.out.println("** pck = " + DECoder.class.getPackage().getName()) ;
			System.out.println("** class = " + DECoder.class.getName()) ;
			DLog.log("entering f, x=",0) ;
			f0DEC() ;
			DLog.log("end of f, x=",0) ;
		}
		
		static public void f0DEC() throws Exception  { 
			DLog.log("executing f0, x=",0) ; 
		}
	}
	
	public static void main(String[] args) throws Exception {
		
		//System.out.println(">>" + (new DECoder()).getClass().getName()) ; 
		
		TestUntracedCall z = new TestUntracedCall() ;
		System.out.println("** " + z.getClass().getName()) ; 
		TLog.initializeLogger() ;
   		z.f() ;
   		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		DECoder.fDEC() ; DLog.tick() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;		
 	}
	
}
