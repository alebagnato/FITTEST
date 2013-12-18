package eu.fittest.tloglib ;

import java.util.* ;
import java.io.* ;

/**
 * A simple Logger. An instance of this class is a logger. The logging functions
 * will however offered as static API that will execute on a single instance of
 * a thread-split logger (using the singleton pattern).
 * 
 * Logs produced by this logger are saved in files named as either of these: 
 * 
 *     log_qualifiedClassName!methodName!tid.txt
 *     log_!tid.txt
 *     
 * Where tid is a thread-id. The name can also be suffixed with a time-stamp.
 *     
 */
public class XLog {

   /**
    * Implementing a singleton pattern.
    */
   static private BasicThreadSplitLogger logger = null ;
   
   
   /**
    * Logging API. Create a new log-file for the current thread. If the thread already
    * has a log-file associated to it, it will be closed, and then a new one will be 
    * created. The created log-file will be named: 
    * 
    *     log_className!methodName!tid_timestamp.txt
    *     log_!methodName!tid_timestamp.txt
    *     log_!tid_timestamp.txt
    *     
    * where tid is the ID of the current thread; and _timestamp is optional.
    * 
    * If debug is set to true, then logged strings are echoed to the error console.
    */
   public static synchronized void initializeLogger(
		   String qualifiedClassName, 
		   String methodName,
		   boolean timestamped,
		   boolean debug )
   {
	   if (logger==null) { 
		   logger = new BasicThreadSplitLogger() ;
		   logger.echo = debug ;
	   }
	   closeLogger() ;
	   logger.linkLogFileToThisThread(LogFileName.mkXLogFileName(qualifiedClassName, methodName, timestamped)) ;
   }
   
 	
   /**
    * Simplified version of initializeLogger. Debug is set to true. The produced file is not
    * time-stamped.
    */
   public static synchronized void initializeLogger(){
	   initializeLogger(null,null,false,true) ;
   }
   
     
   /**
    * Logging API. Will log the static string s and value v.
    */
   public static void log(String s, Object v) { logger.writeln(s + " <" + v + ">") ; }
   
   /**
    * Logging API. Log a static string s.
    */
   public static void log(String s) { logger.writeln(s) ; }
   
   /**
    * Logging API. Close the log-file of the current thread. After this, we can no 
    * longer log into that file. 
    */
   public static void closeLogger(){
	   logger.closeThisThreadLogging() ;
   }


   // tests
   public static void main(String[] args) {
	   initializeLogger("eu.fittest.tloglib.XLog","main",true,true) ;
	   log("hello!") ;
	   log("test",args.length) ;
   }

}