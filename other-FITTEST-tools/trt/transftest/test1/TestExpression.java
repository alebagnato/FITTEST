import eu.fittest.tloglib.*;

public class TestExpression {

	boolean f(int x) { XLog.log("hello") ; return x==0 ; }
	
	void testConj(int x) {
		boolean a = x>0 && x>1 ;
		a = f(x) && x>0 ;
		a = x>0 && f(x) ;
		a = f(x) && f(x) ;
	}
	
	void testDisj(int x) {
		boolean a = x>0 || x>1 ;
		a = f(x) || x>0 ;
		a = x>0 || f(x) ;
		a = f(x) || f(x) ;
	}
	
	void testNegation(int x) {
		boolean a = ! (x>1) ;
		a = ! f(x) ;
	}
	
	boolean g(int x) {
		boolean a = x>0 && x>-1 ;
		boolean b = x>0 && f(x) ;
		boolean c = f(x) && x>0 ;
		boolean d = x>0 || f(x) ;
		boolean e = f(x) || x>0 ;
		boolean f = f(x) ;
		boolean f2 = ! f(x) ;
		boolean g = f(x) ? x>0 : false ;
		boolean h = x>0 ? f(x) : false ;
		boolean i = x>0 ? true : f(x) ;
		return i ;
	}
	
	public static void main(String[] args) {
		XLog.initializeLogger() ;
		new TestExpression().g(10) ;
		XLog.closeLogger() ;
		TLog.printDebug() ;
	}
}
