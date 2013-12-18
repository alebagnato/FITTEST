import eu.fittest.tloglib.* ;

public class TestPolymorph {
	
	static public class A {
		public void foo(int x) { }	
		static public void fooDEC() throws Exception { }
	}
	
	static public class B1 extends A {
	}

	static public class B2 extends A {
		public void foo(int x) { TLog.log("I am B2.foo()") ; }	
		static public void fooDEC() throws Exception { DLog.log("I am B2.foo()",14) ; }
	}
	
	static public class C extends B1 {
		public void foo(int x) { 
			System.out.println("** calling foo of C") ;
			TLog.log("I am C.foo()") ; 
		}
		static public void fooDEC() throws Exception { DLog.log("I am C.foo()",19) ; }
	}

	static public class D extends C {
		int x ;
		public void foo(int x) { this.x = x ; }
		static public void fooDEC() throws Exception { }
	}
	
	public void f1(A a) { 
		// we just want to do a.foo(0); this gives the following tagging:
		Class[] variants = {D.class, C.class ,B1.class, B2.class, A.class} ;
		((A) TLog.polycall(a,variants)).foo(0) ;
	}
	
	static public void f1DEC() throws Exception {
		Class[] variants = {D.class, C.class ,B1.class, B2.class, A.class} ;
		String[] decoderNames = {"fooDEC","fooDEC","fooDEC","fooDEC","fooDEC"} ;
		DLog.polyCall(variants, decoderNames) ;		
		
		//int code = DLog.pop(3) ;
		//switch (code) {
		//case 0 : /* D */  D.fooDEC()  ; break ;
		//case 1 : /* C */  C.fooDEC()  ; break ;
		//case 2 : /* B1 */ B1.fooDEC() ; break ;
		//case 3 : /* B2 */ B2.fooDEC() ; break ;
		//case 4 : /* A */ A.fooDEC() ;
		//}
	    
	}
	
	public void f2(B1 b) { 
		// original is b.foo(0); this is the tagging:
		Class[] variants = {D.class, C.class ,B1.class} ;
		((B1) TLog.polycall(b,variants)).foo(0) ;
	}
	
	
	static public void f2DEC() throws Exception {
		Class[] variants = {D.class, C.class ,B1.class} ;
		String[] decoderNames = {"fooDEC","fooDEC","fooDEC"} ;
		DLog.polyCall(variants, decoderNames) ;
		
		//int code = DLog.pop(2) ;
		//switch (code) {
		//case 0 : /* D */  D.fooDEC() ;  break ;
		//case 1 : /* C */  C.fooDEC() ;  break ;
		//case 2 : /* B1 */ B1.fooDEC() ; 
		//}
	}
	
	public void f3(D d) { 
		// D has no subclass; so no multiple run-type versions of foo is possible.
		// So, no encoding is needed.
		d.foo(0) ; 
	}
	
	
	static public void f3DEC() throws Exception {
		D.fooDEC() ;
	}
	
	public static void main(String[] args) throws Exception {
		System.out.println("** TestPolymorph") ; 
		TLog.initializeLogger() ;
		
		TestPolymorph z = new TestPolymorph() ;
		z.f1(new A()) ; 
		z.f1(new B1()) ; 
		z.f1(new B2()) ; 
		z.f1(new C()) ; 
		z.f1(new D()) ;
		
		z.f2(new B1()) ; 
		z.f2(new C()) ; 
		z.f2(new D()) ;
		
		z.f3(new D()) ;
        
		
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		f1DEC() ; 
		f1DEC() ;  
		f1DEC() ; 
		f1DEC() ; 
		f1DEC() ;
		
		f2DEC() ; 
		f2DEC() ;  
		f2DEC() ;
		
		f3DEC() ;
		
		DLog.tick();
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}
	
}
