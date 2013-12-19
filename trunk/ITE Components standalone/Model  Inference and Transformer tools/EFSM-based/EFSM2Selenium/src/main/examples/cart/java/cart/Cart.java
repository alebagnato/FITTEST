package cart;

public class Cart {
  
    private int n = 0;


    public Cart() {
	System.out.println("Cart()");
    }
    
    
    public void add(int c) {
	System.out.println("add("+c+")");
        
        if (!(n >= 0 && c > 0)) {
		throw new RuntimeException("can't call add in the state n="+n+" or with c="+c);
	}
        
        n += c;
        
       
        
    }
    
    public void rem(int c) {
	System.out.println("rem("+c+")");
        
        // assert n-c >= 0;
        if (!(n-c >= 0 && c > 0)) {
		throw new RuntimeException("can't call rem in the state n="+n+" or with c="+c);
	}
        
        n -= c;
        
       
    }
    
    public void pay() {
	System.out.println("pay()");
        
        // assert n > 0;
        if (!(n > 0)) {
		throw new RuntimeException("can't call pay in the state n="+n);
	}
        
        n = -1;
        
      
        
    }
    
    public int n() {
        return n;
    }

}
