package eu.fittest.tloglib;

import eu.fittest.tloglib.error.* ;
import eu.fittest.tloglib.event.* ;
import java.lang.reflect.*;
import java.util.*;
import java.io.*;

/**
 * Logging functions/APIs for log-decoders. Most methods here should not be
 * called manually; they are called by generated decoders. The produced decoded
 * log is saved in a file named:
 * 
 *    dlog_qualifiedClassName!methodname!MID!timestamp.txt
 * 
 * @author Wishnu Prasetya
 *
 */
public class DLog {
	
	public static boolean DEBUG = false ;
	// if DEBUG is set to true, decoded logs will also be echoed to the
	// copy-var below:
	public static String copy = "" ;
	
	// To hold the encoded-log read from the log file.
	private String encodedLog ;
	// To hold the event-log read from the log file.
	private String eventlog ;
    // To hold the parsed list of events, these are to be read from eventLog
	private EventList eventlist ;
	// the file-name where the decoded log will be written
	private String decodedLogName ;
	
	// where we are now as we read the encoded log
	private int current = 0 ;  
	// length of encoded log
	private int N ;  
	// length of decoded log
	private int K = 0 ;  
	// length of original log, without tracing info
	private int orgK = 0 ;
	// count the number of call to log(..)
	private int orgNumOfLogCall = 0 ;
	
	private boolean pendingNewLine = false ;

	// progress counter
	private int prc = 0 ; 
	
	private DLog(String enclog, String eventlog) {
		encodedLog = enclog ;
		this.eventlog = eventlog ;
		eventlist = new EventList(eventlog) ;
		N = encodedLog.length() ;
		decodedLogName = "dlog.txt" ;
	}
	
	private static Map<Thread,DLog> loggers = new HashMap<Thread,DLog>() ;
	
	private static DLog getLogger() {
		DLog lg = loggers.get(Thread.currentThread()) ;
		assert (lg != null) ;
		return lg ;
	}
	
	/**
	 * API. Create a new decoder for the current thread; it will set to
	 * decode directly from the given strings.
	 */
	public static void initialize(String enclog, String eventlog) {
		DLog lg = new DLog(enclog,eventlog) ;
		loggers.put(Thread.currentThread(), lg) ;
	}
	
	
	/**
	 * API. Create a new decoder for the current thread. The encoded-log
	 * will be read from the specified path and file name. This will also
	 * read the corresponding events-log.
	 * 
	 */
	public static void loadAndInitialize(String path, String bLogName) throws IOException {
		String bFn = path + "/" + bLogName ;
		System.out.println("**Blog file = " + bFn) ;
		String eFn = path + "/" + LogFileName.mkTheCorrespondingEvLogName(bLogName) ;
		System.out.println("**Evlog file = " + eFn) ;
		String encodedlog = FileUtils.readFileToString(bFn) ;
		String evlog = "" ;
		try {
		  evlog = FileUtils.readFileToString(eFn) ;
		}
		catch (Exception e) {
			System.out.println("** WARNING: no event-log is found (it could be that none was generated).") ;
		}
		initialize(encodedlog,evlog) ;
		//System.out.println("**Encoded log = " + encodedlog) ;
		//System.out.println("**Events  log = " + evlog) ;
		String dFn = path + "/" + LogFileName.mkTheCorrespondingDLogFileName(bLogName) ;
		FileUtils.removeFile(dFn) ;
		getLogger().decodedLogName = dFn ;
	}
	
	/**
	 * API. Create a new decoder for the current thread. The given reg-exp is
	 * used to identify the name of the encoded-log to load. If there are multiple
	 * matches, the maximum lexicographic one is chosen. If none is found, and
	 * exception is thrown. 
	 * 
	 * This will load the encoded and the corresponding event logs, then innitialize
	 * the decoder.
	 */
	public static void loadOnPatternAndInitialize(String path, String regexp) throws IOException {
		String bLogName = FileUtils.getMatchingFileName1(path, regexp) ;
		if (bLogName == null) throw new IOException("No matching log-file is found!") ;
		loadAndInitialize(path,bLogName) ;
	}
	
	/**
	 * API. Call this as the last decoding step.
	 */
	public static void closeDecoder() throws Exception{
        tick() ; // give a final tick	
        DLog lg = getLogger() ;
		FileUtils.fclose(lg.decodedLogName) ;
		printStatistics() ;
	}
	
	/**
	 * API.
	 */
	public static void tick() throws Exception { 
		getLogger().check() ;
		getLogger().prc ++ ;
	}
	
	/**
	 * Replay all events that are currently mature (in the same order as they
	 * appear)
	 */
	private void check() throws Exception {
		if (eventlist.isMature(prc)) {
			eventlist.replay() ; 
			check() ;
		}
	}
	
	synchronized private static void debugLogCopy(String s) {
		if (DEBUG) copy += s ;
	}
	
	synchronized public static void printDebug() {
		System.out.println("**Decoded log:\n" + copy) ;	
	}
	
	public static void write(String s) { 
	   DLog lg = getLogger() ;
	   if (lg.pendingNewLine) {
		   xwrite("\n" + s) ;
		   lg.pendingNewLine = false ;
	   }
	   else xwrite(s) ;
	}
	
	public static void xwrite(String s) { 
		DLog lg = getLogger() ;
		FileUtils.fwrite(lg.decodedLogName,false,s) ;
		lg.K += s.length() ;
		int i = s.indexOf('@') ;
		if (i>=0) lg.orgK += s.length() - i - 1 ;
		debugLogCopy(s) ;
	}
	   
	public static void writeln(String s) { write(s + "\n") ; }
	
	public static void logLnNr(int linenr) throws Exception { 
		logLnNr("" + linenr) ;
	}
	
	public static void logLnNr(String lineInfo) throws Exception { 
		String info = lineInfo ;
		int k = lineInfo.lastIndexOf(':') ;
		if (k>=0) {
			String className = lineInfo.substring(0,k) ;
			String lineNr = lineInfo.substring(k) ;
			info = className + ".java" + lineNr ;
		}
		write("(" + info + ")") ;
	}
	
	public static void log(String s, int linenr) throws Exception {
		log(s,"" + linenr) ;
	}
	
	public static void log(String s, String linenrInfo) throws Exception {
		tick() ;
		logLnNr(linenrInfo) ;
		DLog lg = getLogger() ;
		write("@" + s) ;
		lg.pendingNewLine = true ;
		lg.check() ; 
		lg.orgNumOfLogCall++ ;
		write("") ;
	}
	
	public static boolean pop(int trueline, int falseline) throws Exception {
		return pop("" + trueline, "" + falseline) ;
	}
	
	public static boolean pop(String trueline, String falseline) throws Exception {
		return pop(null,trueline,falseline) ;
		/*
		tick() ;
		DLog lg = getLogger() ;
		boolean r = lg.pop() ;
		if (r) { logLnNr(trueline) ; write("T ") ; }
		else { logLnNr(falseline) ; write("F ") ; }
		lg.numOfEntries++ ;
		return r ;
		*/
	}
	
	public static boolean pop(String guardline, String trueline, String falseline) throws Exception {
		tick() ;
		DLog lg = getLogger() ;
		if (guardline!=null) { logLnNr(guardline) ; write("COND ") ; }
		boolean r = lg.pop() ;
		if (r) { logLnNr(trueline) ; write("T ") ; }
		else { logLnNr(falseline) ; write("F ") ; }
		return r ;
	}
	
	private boolean pop() {
		if (current>=N) {
			throw new LogDecodingError("Trying to pop, but there is no more bit to pop.") ;
		}
		//System.out.println("** read encoded log = " + encodedLog) ;
		String b = "" + encodedLog.subSequence(current, current+1) ;
		//System.out.println("** current bit = " + b) ;
		if (b.equals(TLog.ONE)) {
			current++ ;	
			return true ;
		}
		else if (b.equals(TLog.ZERO)){
			current++ ;	
			return false ;
		}
		else {
			throw new LogDecodingError("Trying to pop, but input is in wrong format, at position " + current) ;
		}		
	}
	
	/**
	 * Pop k-bits, and convert the result to an integer.
	 */
	public static int pop(int k) throws Exception {
		tick() ;
		DLog lg = getLogger() ;
		int code = 0 ;
		for (int i=0; i<k; i++) {
			code = 2*code ;
			if (lg.pop()) code++ ;
		}
		return code ;
	}
	
	public static void enterFinally() {
		DLog lg = getLogger() ;
		if (!lg.eventlist.eventlist.isEmpty() 
				&& lg.eventlist.eventlist.get(0) instanceof FinallyEvent
				&& lg.eventlist.isMature(lg.prc))
			lg.eventlist.eventlist.remove(0) ;
		//else
		//	throw new Error("Decoding error: entering a finally section, but no finally-event is found.") ;
	}
	
	/**
	 * For decoding a polymorphic call.
	 */
	public static void polyCall(Class[] variants, String[] decoderNames) throws Exception {
		int k = BitGenerator.countBitsCombination(variants.length) ;	
		int code = pop(k) ;
		Class c = variants[code] ;
		//System.out.println("** code = " + code) ;
		//System.out.println("** class = " + c) ;
		String decoderName = decoderNames[code] ;
		Method decoder = c.getMethod(decoderName, new Class[0]) ;
		DLog.writeln(" POLYCALL: " + c.getName() + "." + decoderName.substring(4)) ;
		decoder.invoke(null, null) ;
	}
	
	public static void printEncodedLog() {
		DLog lg = getLogger() ;
		System.out.println("\n-------------") ;
		System.out.println("**Encoded : " + lg.encodedLog) ;
		System.out.println("**Eventlog: " + lg.eventlog)   ;
		System.out.println("-------------") ;
	}
	
	
	public static DLogStatistics getStatistics(){
		DLog lg = getLogger() ;
		return new DLogStatistics(lg.encodedLog.length(),lg.eventlog.length(),lg.K,lg.orgK,lg.orgNumOfLogCall) ;
	}
	
	public static void printStatistics() {
		DLogStatistics stat = getStatistics() ;
		System.out.println("\n-------------") ;
		System.out.println(getLogger().decodedLogName) ;
		stat.printStatistics() ;
	}

	public static void main(String[] args) throws Exception {
		initialize("1011",":1:2:999:6:2:3") ;
		DEBUG = true ;
		log("start ","1") ;
		pop("2","3") ;
		pop("3","4") ;
		pop("4","5") ;
		pop("5","6") ;
		log("j=","6") ;	
		tick() ;
		printDebug() ;
		printStatistics() ;
	}
	
}
