import eu.fittest.tloglib.*;

public class TestAbstractMethod {
	
	static abstract class A {
		public abstract int foo() ;
	}
	
	static class B extends A {
		
		public int foo() {
			XLog.log("foo...") ; return 99 ;
		}
		
	}

	static class C extends B {
		
		public int foo() {
			return 88 ;
		}
		
	}
	
	public void test1(A x) { x.foo() ; }
	public void test2(B x) { x.foo() ; }
	public void test3(C x) { x.foo() ; }
	
	
    public static void main(String args[]) {
	   XLog.initializeLogger() ;
	   new TestAbstractMethod().test1(new B()) ;
	   new TestAbstractMethod().test1(new C()) ;
	   new TestAbstractMethod().test2(new B()) ;
	   new TestAbstractMethod().test2(new C()) ;
	   new TestAbstractMethod().test3(new C()) ;
	   XLog.closeLogger() ;
	   TLog.printDebug() ;
    }


}
