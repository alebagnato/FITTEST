package PkgA.PkgB;

import eu.fittest.tloglib.* ;
import PkgA.*  ;

public class TestPackage2 {
	
    public static void g1() { TestPackage1.f1() ; }
	
	public static void g2() {  TestPackage1.f2() ; }
	
	public static void main(String[] args) {
		XLog.initializeLogger() ;
		g1() ;
		g2() ;
		XLog.closeLogger() ;
		TLog.printDebug() ;
	}

}
