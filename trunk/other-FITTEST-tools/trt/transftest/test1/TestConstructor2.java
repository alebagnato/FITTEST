import eu.fittest.tloglib.* ;

public class TestConstructor2 {
	
	static public class Animal {		
		public Animal() { 
			System.out.println("-- Animal") ;
		}
	}
	
	static public class Duck extends Animal {
		public Duck() {
			super() ;
			XLog.log("Creating a duck",99) ;
			System.out.println("-- Duck") ;
		}
	}
	
	static public class RedDuck extends Duck {
		public RedDuck() {
			super() ;
			System.out.println("-- Red duck") ;
		}
	}
	
	static public class WhiteDuck extends Duck {
		public WhiteDuck() { System.out.println("-- White duck") ; }
	}
	
	static public class DottedRedDuck extends RedDuck {
		public DottedRedDuck() { 
			System.out.println("-- Dotted red duck") ;
		}
	}
	
	public static void main(String[] args){
		System.out.println("** TestConstructor") ; 
		XLog.initializeLogger() ;

		new Animal() ;
		new Duck() ;
		new RedDuck() ;
		new WhiteDuck() ;
		new DottedRedDuck() ;
		
		XLog.closeLogger() ;
		TLog.printDebug() ;
	}
	
}
