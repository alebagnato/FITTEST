package tobetested;

public class UnderTest {
	
	private int x = 0;
	
	public void first(int y) {
		
		if (x == 0) {
			
			x = y;
			
		} 
		
	}
	
	public void second(int y) {
		
		if (x == y) {
			
			x = 0;
			
		}
		
	}
	
	
}