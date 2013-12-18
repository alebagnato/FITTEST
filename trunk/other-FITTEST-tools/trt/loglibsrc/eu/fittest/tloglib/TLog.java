package eu.fittest.tloglib;

import java.util.*;

import eu.fittest.tloglib.event.*;

/**
 * Logging functions/APIs for tagged application. Most methods here should not be 
 * called manually by a class. They are to be injected automatically to the target
 * API. 
 * 
 * Logging through this class will produce an encoded log and the corresponding event
 * log. The 0/1 encoded log will be named:
 * 
 *     blog_className!methodName!MID!tid_timestamp.txt
 *     blog_!methodName!MID!tid_timestamp.txt
 *     blog_!MID!tid_timestamp.txt
 * 
 * where MID is an integer unique ID assigned to the method name by the encoder. The
 * corresponding event-log will have the same name, but prefixed with evlog_
 *
 * @author Wishnu Prasetya
 *
 */
public class TLog {
	
	// Singleton, to hold the thread-split logger.
	private static BasicThreadSplitLogger logger ;
	
	// Every thread will get its own progress counter. This counter is updated
	// by every call to tick.
	private static Map<Thread,Integer> prcs ;
	
	// And every thread will get its own iteration-buffer. Such a buffer is 
	// used to collapse the bit-string produced by the logging of a loop to
	// produce shorter a bit-string.
	private static Map<Thread,IterationLogBuffer> iterationLogBuffers ;

	public static boolean DEBUG = true ;
	// copies of logs, for debugging:
	private static String copy = "" ;
	private static String eventLogCopy = "" ;
	
    /**
	 * Logging API. Create a new log-file for the current thread. If the thread already
	 * has a log-file associated to it, it will be closed, and then a new one will be 
	 * created. The created log-file will be named: log_className_methodId_tid_timestamp.txt
     * 
     * The methodId is a unique-number that belongs to the method that calls this
     * initialization.
     * 
	 * If debug is set to true, the debug-mode is turned on, and logged strings will
	 * be copied to some internal buffers, which can be queried later. Note that the
	 * debug-mode as well as the debug-buffers are currently global!
	 */
	public static synchronized void initializeLogger(
			   String qualifiedClassName, 
			   String methodName,
			   int methodId,
			   boolean debug
			) {
		if (logger==null) { 
			   logger = new BasicThreadSplitLogger() ;
			   // don't make the logger to echo; we'll add a custom debugging:
			   logger.echo = false ;
			   DEBUG = debug ;
			   iterationLogBuffers = new HashMap<Thread,IterationLogBuffer>() ;
			   prcs = new HashMap<Thread,Integer>() ;	   
		}
		closeLogger() ;
		logger.linkLogFileToThisThread(LogFileName.mkBLogFileName(qualifiedClassName, methodName, methodId,true)) ;
	}

	/**
	 * Simplified version of initializeLogger. Debug is turned on. The logs are saved in
	 * blog.txt and evlog.txt.
	 */
	public static synchronized void initializeLogger() {
		initializeLogger(null,null,0,true) ;
	}
	
	/**
	 * Logging API. Close the log-file of the current thread. After this, we can no 
	 * longer log into that file. 
	 */
	public static void closeLogger() {	
		flushIterationBuffer() ;
		clearMaps() ;
		logger.closeThisThreadLogging() ;	
	}
		
	private static void clearMaps() {		
		Thread t = Thread.currentThread() ;
		iterationLogBuffers.remove(t) ;
		prcs.remove(t) ;
	}
	
	// Flushing iteration buffer.
	public static void flushIterationBuffer() {
		Pair<String,String> p = getIterationLogBuffer().flush() ;
		assert p != null ;
		write(p.fst) ;
		if (p.snd.equals("")) return ;
		writeEvent(p.snd) ;
	}
	
	public static IterationLogBuffer getIterationLogBuffer() {
		Thread t = Thread.currentThread() ;
		IterationLogBuffer ib =  iterationLogBuffers.get(t) ;
		if (ib == null) {
			ib = new IterationLogBuffer() ;
			iterationLogBuffers.put(t, ib) ;
		}
		return ib ;
	}
	
	/**
	 * Logging API. Will print the content of the debug buffers to the error console.
	 */
	public static void printDebugBuffers() {
		if (copy.equals("") && eventLogCopy.equals(""))
			System.err.println("** DEBUG TLog: nothing to print!") ;
		else {
			System.err.println("** DEBUG TLog, bitlog: " + copy) ;
			System.err.println("** DEBUG TLog, eventlog: " + eventLogCopy) ;		
		}
	}
		
	/**
	 * Logging API. Return the content of debug-copy of the encoded log.
	 */
	public static String getDebugLogCopy() { return "" + copy ; }

	/**
	 * Logging API. Return the content of the debug-copy of the event log.
	 */
	public static String getDebugEventLogCopy() { return "" + eventLogCopy ; }
	
	// return the progress-count of the current thread. Access to it is required
	// from another class --> protected.
	protected static int getProgressCount() {
		Thread t = Thread.currentThread() ;
		Integer i = prcs.get(t) ;
		if (i == null) {
			i = 0 ;
			prcs.put(t, 0) ;
		}
		return i ;
	}
	
	// to set the value of the progress-count of the current thread. Access to it is required
	// from another class --> protected.
	protected static void setProgressCount(int k) {
		Thread t = Thread.currentThread() ;
		prcs.put(t,k) ;
	}
	
	private static void increaseProgressCount(int delta) {
		setProgressCount(getProgressCount() + delta) ;
	}
		

	private static void debugLogCopy(String s) {
		if (DEBUG) copy += s ;
	}
	
	private static void debugEventLogCopy(String s){
		if (DEBUG) eventLogCopy += s ;
	}
	
	/**
	 * Logging API. Used to print out the content of the debug-copy buffer.
	 */
	public static void printDebug() {
		System.out.println("**DEBUG, binary log: " + copy) ;
		System.out.println("**DEBUG, event log : " + eventLogCopy) ;	
	}
	
	// write a string to the current's thread logger.
	private static void write(String s) { 
		String logfile = logger.getLogFileNameOfThisThread() ;
		if (logfile == null) {
			assert s == null || s == "" ;
			return ;
		}
		FileUtils.fwrite(logfile, false, s) ;
		debugLogCopy(s) ;
    }
	  
	// to write to the event's log-file belonging to the current thread
	private static void writeEvent(String value) { 
		String blogfile = logger.getLogFileNameOfThisThread() ;
		FileUtils.fwrite(LogFileName.mkTheCorrespondingEvLogName(blogfile), false, value) ;
		debugEventLogCopy(value) ;
    }
	   
	static public final String ONE  = "1" ;
	static public final String ZERO = "0" ;

	// counter to generate fresh loop-id variables.
	private static long loopId = -1 ;

	/**
	 * Logging API, to advance the progress counter belonging to the current thread.
	 */
	public static void tick() { increaseProgressCount(1) ; }
	
	/**
	 * Logging API, to push a boolean value 0/1 to the log-file that belongs to
	 * the current thread.
	 */
	public static boolean push(boolean t) {
		tick() ;
		IterationLogBuffer ib = getIterationLogBuffer() ;
		if (ib.isEmpty()) write (encode(t)) ;
		else ib.appendTop(encode(t)) ;
		return t ;
	}

	private static String encode(boolean t) {
	      if (t) return ONE ; else return ZERO ;
	}
	
	private static String encode(String bitstring) {
		String z = "" ;
		for (int i=0; i<bitstring.length(); i++)
	        if (bitstring.charAt(i)=='1') z += ONE ;
	        else z += ZERO ;
		return z ;
	}

	/**
	 * Logging API, to push a boolean string to the log-file that belongs to
	 * the current thread.
	 */
	public static void push (String bitstring) {
		tick() ;
		IterationLogBuffer ib = getIterationLogBuffer() ;
		if (ib.isEmpty()) write (encode(bitstring)) ;
		else ib.appendTop(encode(bitstring)) ;
	}

	/**
	 * Flush the whole content of the iteration buffer, and write it to the
	 * log file.
	 */
	private static void commit() {
		tick() ; 
		flushIterationBuffer() ;
	}
	 
    /**
	 * Logging API. To log the static string s and value v.
	 */
	public static void log(String s, Object v) { 
		commit() ; 
		String val = "" ;
		if (v==null) val = "null" ; else val += v ;
		DynamicValueEvent e = new DynamicValueEvent(getProgressCount(),val) ;
		String val_ = e.serialize() ;
		writeEvent(val_) ;
	}
	
    /**
	 * Logging API. To log the static string s.
	 */
	public static void log(String s) { 
		commit() ;
	}
	
    /**
	 * Logging API. To log an exception.
	 */
	public static void logE(Throwable e) {
		ExceptionEvent e_ = new ExceptionEvent(getProgressCount(),e.getClass().getName()) ;
		String es = e_.serialize() ;
		IterationLogBuffer ib = getIterationLogBuffer() ;
		if (ib.isEmpty()) writeEvent(es) ;
		else ib.appendEventTop(es) ;
	}
	
	/**
	 * Logging API. To get a unique ID to distinguish different invocation of
	 * the same loop.
	 */
	public static synchronized long getLoopId() {
	       loopId++ ;
	       return loopId ;
	}

	/**
	 * The special push operation for loop-guard.
	 */
	public static boolean ipush(long loopid, boolean t) {
		//System.out.print("<<" + prc + ">>") ;
		/*
		getIterationLogBuffer().addIteration(loopid) ;
		boolean val = push(t) ;
		if (!val) flushIterationBuffer() ; // always flush when we exit the loop
		return val ;
		*/
		// WP: flushing when the loop exit results in spamming in a log-relevant nested loop,
		// even when the loop turns out not to produce any logging. So we turn that off:
		getIterationLogBuffer().addIteration(loopid) ;
		if (!t) getIterationLogBuffer().clearToId(loopid) ;
		push(t) ;
		return t ;
	 }
	
	/**
	 * Variation of ipush, to be used at the end of an enhanced For. (special case)
	 */
	public static void ipushWithBreakCheck(boolean didBreak, long loopid) {
		if (didBreak) return ;
		ipush(loopid,false) ;
	 }

	
	public static void untracedCall(String decoderName) { 
		String e = (new UntracedCall(getProgressCount(),decoderName)).serialize() ;
		IterationLogBuffer ib = getIterationLogBuffer() ;
		if (ib.isEmpty()) writeEvent(e) ;
		else ib.appendEventTop(e) ;
	}

	public static Object polycall(Object o, Class[] cs) {
		int k = BitGenerator.countBitsCombination(cs.length) ;
		BitGenerator bits = new BitGenerator(k) ;
		for (int i=0; i<cs.length; i++) {
			String code = bits.next() ;
			if (cs[i].isInstance(o)) {
				//System.out.print("** o = " + o.getClass()) ;
				//System.out.println("cs[" + i + "] = " + cs[i] ) ;				
				push(code) ;
				break ;
			}
		}
       return o ;
    }
	
	public static Object polycall(Object o, String[] classNames) {
		Class[] classes = new Class[classNames.length] ;
		for (int i=0; i<classes.length; i++)
			try { classes[i] = Class.forName(classNames[i]) ; }
		    catch (ClassNotFoundException e) {
		    	throw new Error(e) ;
		    }		
		return polycall(o,classes) ;
    }
	
	public static void enterFinally(){
		//System.out.println("## enter-finally") ;
		String f = (new FinallyEvent(getProgressCount())).serialize() ;
		writeEvent(f) ;
	}
	
	public static void main(String[] args) {
		initializeLogger("A.B.C","foo",99,true) ;
		log("start","start") ;
		long id1 = getLoopId() ;
		for (int i=0; ipush(id1,i<5); i++) {
			if (push(i==3)) log("i=",i) ;
			long id2 = getLoopId() ;
			for (int j=0; ipush(id2,j<5); j++) {
				if (push(j==2)) log("j=",j) ;
			}
		}
		log("end","end") ;
		printDebug() ;
	}

}
