import eu.fittest.tloglib.*;

import java.util.*;

public class TestForLoop {

	int f(int x) {
		XLog.log("start f: ") ;
		int j ;
		for (j=0 ; j<x ; j++) {
			if (j==10) XLog.log("j=",j) ;
		} 
		XLog.log("end f.") ;
		return j ;
	}
	
	int f2(int x) {
		XLog.log("start f2: ") ;
		int j ;
		for (j=f(0) ; j+f(0)<x ; j = j + f(1)) {
			if (j==10) XLog.log("j=",j) ;
		} 
		XLog.log("end f2.") ;
		return j ;
	}
	
	int g(List<Object> s) {
		XLog.log("start g: ") ;
		int i=0 ;
		for(Object o : s) {
			if (i==0) XLog.log("first iteration, i=",i) ;
			i++ ;
		}
		XLog.log("i=",i) ;
		XLog.log("end g.") ;
		return i ;
	}
	
	int g2(List<Object> s) {
		XLog.log("start g2: ") ;
		int i=0 ;
		for(Object o : s) {
			if (i==0) XLog.log("first iteration, i=",i) ;
			else { i=99; break ; }
			i++ ;
		}
		XLog.log("i=",i) ;
		XLog.log("end g2.") ;
		return i ;
	}
	
	 public static void main(  String args[]){
		XLog.initializeLogger();
		new TestForLoop().f(12);
		new TestForLoop().f2(12);
		
		List<Object> s = new LinkedList<Object>() ;
		s.add(new Object()) ;
		s.add(new Object()) ;
		s.add(new Object()) ;
		
		
	    new TestForLoop().g(s) ;
	    new TestForLoop().g2(s) ;
	    XLog.closeLogger() ;
		TLog.printDebug();
	 }
}
