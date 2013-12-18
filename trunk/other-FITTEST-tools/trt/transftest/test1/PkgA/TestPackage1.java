package PkgA;
import eu.fittest.tloglib.*;

public class TestPackage1 {
	
	public static void f1() { XLog.log("f1") ; }
	
	public static void f2() {  }
	
	public static void main(String[] args) {
		XLog.initializeLogger() ;
		f1() ;
		f2() ;
		XLog.closeLogger() ;
		TLog.printDebug() ;
	}

}
