package eu.fittest.tloglib;

import java.util.Calendar;

public class LogFileName {

	/**
	 * Create a file name. The name is prefix!tid, where tid is the thread-id
	 * of the current thread. If the boolean-flag is set to true, the generated 
	 * name will be further suffixed by a time stamp.
	 */
	static public String mkLogFileName(String prefix, boolean timestamped) {
		   Calendar c = Calendar.getInstance() ;
		   long tid = Thread.currentThread().getId() ;
		   String name = prefix + "!" + tid ;
		   if (timestamped)
			   name += "_" 
				   + c.get(Calendar.YEAR) + "_" 
		   		   + c.get(Calendar.MONTH) + "_" 
		   		   + c.get(Calendar.DAY_OF_MONTH) + "_"
		   		   + c.get(Calendar.HOUR_OF_DAY) + "_"
		   		   + c.get(Calendar.MINUTE) + "_"
		   		   + c.get(Calendar.SECOND) ;

		   return name + ".txt" ;
	}

	/**
	 * Create a name for a log-file, with no time-stamp suffix.
	 */
	static public String mkLogFileName(String prefix) {
		   return mkLogFileName(prefix,false) ;	   		   
	}
	
	/**
	 * Create a file name for a regular log-file (for logging through XLog). 
	 * Format:
	 *    log_qclassname!mname!tid_timestamp
	 *    log_!mname!tid_timestamp
	 *    log_!tid_timestamp    
	 *    
	 * And the _timestamp part is optional.   
	 *
	 */
	static public String mkXLogFileName(
			String qualifiedClassName, 
			String methodName, 
			boolean timestamped) {	
		String prefix  = "log_" ;
		if (methodName == null) return mkLogFileName(prefix, timestamped) ;	
		if (qualifiedClassName != null) prefix += qualifiedClassName ;
		prefix += "!" + methodName ;
		return mkLogFileName(prefix,timestamped) ;
	}

	/**
	 * Create a file name for a 0/1-encoded log-file (for logging through TLog).
	 * Format:
	 *      blog_qclassname!mname!mid!tid_timestamp
	 * 
	 * where mid is an integer unique method id.
	 */
	static public String mkBLogFileName(
			String qualifiedClassName, 
			String methodName, 
			int methodId,
			boolean timestamped) {	
		String mname = null ;
		if (methodName != null) mname = "" + methodName + "!" + methodId ;
		return "b" + mkXLogFileName(qualifiedClassName,mname,timestamped) ;
	}
	
	/**
	 * Create file name for the event-log-file that corresponds to the
	 * given encoded log name.
	 * Format:
	 *      evlog_qclassname!mname!mid!tid_timestamp  
	 */
	static public String mkTheCorrespondingEvLogName(String bLogName) {
		return "ev" + bLogName.substring(1) ;
	}
	
	/**
	 * Extract the class-name from a given encoded-log name.
	 */
	static public String getClassName(String bLogName) {
		String s1 = bLogName.substring(5) ;
		String cname = s1.substring(0, s1.indexOf('!')) ;
		return cname ;
	}
	
	/**
	 * Extract the Method-ID from a given encoded-log name.
	 */
	static public int getMethodId(String bLogName) {
		String s1 = bLogName.substring(bLogName.indexOf('!')+1) ;
		String s2  = s1.substring(s1.indexOf('!')+1) ;
		String mid = s2.substring(0, s2.indexOf('!')) ;
		return Integer.parseInt(mid) ;
	}
	
	
	/**
	 * Create a file name for an decoded-log-file (decoded through DLog). 
	 * Format:
	 *      dlog_qclassname!mname!mid!tid_timestamp  
	 */
	static public String mkTheCorrespondingDLogFileName(String bLogName) {
		return "d" + bLogName.substring(1) ;
	}
	
	
	public static void main(String[] args) {
		String blog = mkBLogFileName("A.B.C","foo",99,true)  ;
		System.out.println("encoded log = " + blog) ;
		System.out.println("event log   = " + mkTheCorrespondingEvLogName(blog)) ;
		System.out.println("decoded log = " + mkTheCorrespondingDLogFileName(blog)) ;
		
		System.out.println("Class name = " + getClassName(blog)) ;
		System.out.println("MID = " + getMethodId(blog)) ;
	}
	
}
