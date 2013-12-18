import eu.fittest.tloglib.* ;

public class TestConstructor3 {
	
	TestConstructor3() { }
	
	TestConstructor3(int x) {
		XLog.log("Calling constructor with x = ", x) ;
	}

	TestConstructor3(String s) { 
		this(s.length()) ;
	}

	public static void main(String[] args){
		System.out.println("** TestConstructor") ; 
		XLog.initializeLogger() ;

		new TestConstructor3() ;
		new TestConstructor3(99) ;
		new TestConstructor3("99") ;
		
		XLog.closeLogger() ;
		TLog.printDebug() ;
	}
}
