import eu.fittest.tloglib.*;

public class TestTry {

	int f(int x) {
		x++ ;
		try { x++ ; } catch(Exception e) { x=0 ; return x ; }
		finally { x++ ; }
		x++ ;
		return x ;	
	}
	
	int g(int x) {
		XLog.log("starting g") ;
		x++ ;
		try { x++ ; } 
		catch(Exception e) { 
			x=0 ; 
			XLog.log("g: catching exception, x=",x) ;
			return x ; }
		finally { x++ ; }
		x++ ;
		return x ;	
	}
	
	void g2(int x) {
		XLog.log("starting g2") ;
		try { x++ ; return ; } 
		catch (Exception e) { x=0 ; }
		finally { XLog.log("g2: finally") ; }
	}
	
	void g3(int x) throws Exception {
		XLog.log("starting g3") ;
		try { x++ ; throw new Exception() ; } 
		finally { XLog.log("g3: finally") ; }
	}
	
	void g4(int x) {
		try { g3(x) ; } catch (Exception e) { }
	}
	
	 
	public static void main(String args[]) {
	   XLog.initializeLogger() ;
	   new TestTry().f(0) ;
	   new TestTry().g(0) ;
	   new TestTry().g2(0) ;
	   try { new TestTry().g3(0) ; } catch (Exception e) { }
	   new TestTry().g4(0) ;
	   XLog.closeLogger() ;
	   TLog.printDebug() ;
	}
}
