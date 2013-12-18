
public class CobaException {

	static void f1(int x) throws Exception {
		try{
			if (x==0) throw new Exception();
		}
		catch(Exception e) {
			System.out.println("** f1 in handler") ;
			throw new Exception();
		}
		finally {
			System.out.println("** f1 in finally") ;
		}
		System.out.println("** f1 after try-catch-fin") ;
	}
	
	static void f2(int x) throws Exception {
		try{
			if (x==0) throw new Exception();
		}
		catch(Exception e) {
			System.out.println("** f2 in handler") ;
			return ;
		}
		finally {
			System.out.println("** f2 in finally") ;
		}
		System.out.println("** f2 after try-catch-fin") ;
	}
	
	public static void main(String[] args) throws Exception {
		try { f1(0) ; } catch(Exception e) { } 
		f2(0) ;
	}
	
}
