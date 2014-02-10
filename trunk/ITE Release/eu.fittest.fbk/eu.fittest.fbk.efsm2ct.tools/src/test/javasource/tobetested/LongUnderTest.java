package tobetested;

public class LongUnderTest {
	
	private int x = 0;
	
	public void first(int y) {
		
		System.out.print("X");
		
		if (x == y) {
			
			x = y;
			
		} else {
			
			if (x == 1+x) {
				
				x = 1; // unreachable
				
			} else {
				
				x = 0;
				
			}
			
		}
		
	}
	
}