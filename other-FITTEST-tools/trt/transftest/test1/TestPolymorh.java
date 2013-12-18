import eu.fittest.tloglib.TLog;
import eu.fittest.tloglib.XLog;

public class TestPolymorh {
		
		static public class A {
			public void foo(int x) { }	
		}
		
		static public class B1 extends A {
		}

		static public class B2 extends A {
			public void foo(int x) { XLog.log("I am B2.foo()") ; }	
		}
		
		static public class C extends B1 {
			public void foo(int x) { XLog.log("I am C.foo()") ; }
		}

		static public class D extends C {
			int x ;
			public void foo(int x) { this.x = x ; }
		}
		
		public void f1(A a) { 
			a.foo(0) ;
		}
		
		public void f2(B1 b) { 
			b.foo(0) ;
		}
		
		public void f3(D d) { 
			// D has no subclass; so no multiple run-type versions of foo is possible.
			// So, no encoding is needed.
			d.foo(0) ; 
		}
		
		public static void main(String args[]) {
			   XLog.initializeLogger() ;
			   new TestPolymorh().f1(new A()) ;
			   new TestPolymorh().f1(new B1()) ;
			   new TestPolymorh().f1(new B2()) ;
			   new TestPolymorh().f1(new C()) ;
			   new TestPolymorh().f1(new D()) ;
			   
			   new TestPolymorh().f2(new B1()) ;
			   new TestPolymorh().f2(new C()) ;
			   new TestPolymorh().f2(new D()) ;
			   
			   new TestPolymorh().f3(new D()) ;

			   XLog.closeLogger() ;
			   TLog.printDebug() ;
			}
	
}
