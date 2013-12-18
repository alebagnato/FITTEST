import eu.fittest.tloglib.* ;

import java.lang.reflect.*;

public class TestExternalCall {
	
	 static void f1(int x) {
		XLog.log("start f1") ;
		f2(x) ;
		XLog.log("end f1") ;
	 }
	
	 static void f3(Integer x) {
		XLog.log("f3, x=", x) ;
	 }
	 
	 static void f2(int x){
		if (x==0){
			try {
				Class C = TestExternalCall.class ;
				Method m = C.getDeclaredMethod("f3", Integer.class) ;
				// System.out.println("m constructred") ;
				m.invoke(null, x) ;
			}
			catch(Exception e) {
				System.out.println("Fail to call f3..." + e) ;
			}
		}
	}

	public static void main(String[] args) {
		XLog.initializeLogger() ;
		f1(1) ;
		f1(0) ;
		XLog.closeLogger() ;
		TLog.printDebug() ;
	}
}
