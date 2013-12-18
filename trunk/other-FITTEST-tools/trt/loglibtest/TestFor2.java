import eu.fittest.tloglib.* ;

import java.util.*;

public class TestFor2 {
	
	int f(List s) {
		int i=0 ;
		long id1 = TLog.getLoopId() ;
		boolean brk1 = false ;
		for(Object o : s) {
			TLog.ipush(id1,true) ;
			if (TLog.push(i==0)) TLog.log("first iteration, i=",i) ;
			if (TLog.push(i==3)) { 
				//TLog.log("break") ;
				brk1 = true ; // you need to mark when you break the loop!
				TLog.tick(); break ; 
				}
			if (TLog.push(i==s.size()-1)) TLog.log("last iteration, i=",i) ;
			i++ ;
		}
		TLog.ipushWithBreakCheck(brk1,id1) ;
		//TLog.log("end") ;
		TLog.tick() ;
		return i ;
	}
	
	void fDEC() throws Exception {
		while(true) {
			// decoder of the guard "s" --> skip
			if (!DLog.pop(0,1)) break ;
			if (DLog.pop(2,3)) DLog.log("first iteration, i=",8) ;
			if (DLog.pop(4,5)) { 
				//DLog.log("break",5) ;
				DLog.tick(); 
				break ; }
			if (DLog.pop(6,7)) DLog.log("last iteration, i=",12) ;
		}
		//DLog.log("end",15) ;
		DLog.tick() ;
		return ;
	}
	
	public static void main(String[] args) throws Exception {
		TestFor2 z = new TestFor2() ;
		System.out.println("** " + z.getClass().getName()) ;
		TLog.initializeLogger() ;
		List u = new LinkedList() ;
		z.f(u) ; // tesing with empty list first
		u.add(0) ; u.add(99) ; u.add(999) ; 
		z.f(u) ; 
		u.add(11) ; 
		z.f(u) ;
		TLog.closeLogger() ;
		
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		z.fDEC() ;
		z.fDEC() ; 
		z.fDEC() ; 
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}
}
