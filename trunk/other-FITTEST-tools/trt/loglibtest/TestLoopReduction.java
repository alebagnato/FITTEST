import eu.fittest.tloglib.* ;

public class TestLoopReduction {
	
	void f() {
		TLog.log("start","start") ;
		int i = 0 ;
		long id1 = TLog.getLoopId() ;
		while (TLog.ipush(id1,i<5)) {
			if (TLog.push(i==3)) TLog.log("i=",i) ;
			int j=0 ;
			long id2 = TLog.getLoopId() ;
			while (TLog.ipush(id2,j<5)) {
				if (TLog.push(j==2)) TLog.log("j=",j) ;
				j++ ;
			}
			i++ ;
		}
		//TLog.log("end","end") ;
	}

	static void fDEC() throws Exception {
		DLog.log("start ",6) ;
		while (true) {
			if (!DLog.pop(9,19)) break ;
			if (DLog.pop(10,11)) DLog.log("i=",11) ;
			while (true) {
				if (!DLog.pop(13,17)) break ;
				if (DLog.pop(14,15)) DLog.log("j=",15) ;
			}
		}
		//DLog.log("end ",19) ;
	}
	
	public static void main(String[] args) throws Exception {
		TLog.initializeLogger() ;
		TestLoopReduction z = new TestLoopReduction() ;
		z.f() ;
		TLog.closeLogger() ; // always flush at the end of top-level
		
		DLog.initialize(TLog.getDebugLogCopy(), TLog.getDebugEventLogCopy()) ;
		DLog.DEBUG = true ;
		DLog.printEncodedLog() ;
		fDEC() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
 	}

}
