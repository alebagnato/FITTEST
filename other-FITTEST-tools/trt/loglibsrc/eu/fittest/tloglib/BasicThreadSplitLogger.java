package eu.fittest.tloglib;

import java.util.*;


/**
 * Provide a basic thread-split-logger. In such a logger, every thread
 * gets its own unique log-file. This avoids threads to compete for
 * access to a common log-file. 
 * 
 * Clients can log time-stamps to allow the separate log-files to be, 
 * to some degree, linearized. 
 */
public class BasicThreadSplitLogger {

	/**
    * For every thread for which we want to provide logging, we provide a mapping 
    * to the unique name of the log-file that belongs to the thread.
    */
   protected Map<Thread,String> logfilesMap ;
   
   /**
    * If true will cause logged string to be echoed to the error console.
    */
   public boolean echo = true ;
   
   public BasicThreadSplitLogger() {
	   logfilesMap = new HashMap<Thread,String>() ;
   }

   /**
    * Return the log-file name that belongs to the current thread.
    */
   public String getLogFileNameOfThisThread() {
	   return logfilesMap.get(Thread.currentThread()) ;
   }
   
   /**
    * Associate the given filename to be treated as the target log-file
    * for this thread.
    */
   public void linkLogFileToThisThread(String filename) {
	   logfilesMap.put(Thread.currentThread(), filename) ;
   }
   
   /**
    * "Close" the log-file that belongs to the current thread.
    * After this, we cannot log/write into that file anymore.
    */
   public void closeThisThreadLogging() {
	   String f = getLogFileNameOfThisThread() ;
	   FileUtils.fclose(f) ;
	   logfilesMap.remove(Thread.currentThread()) ;
   }
   
   /**
    * To write a string to the log-file belonging to the current thread.
    */
   public void write(String s) { 
	   FileUtils.fwrite(getLogFileNameOfThisThread(),echo,s) ;
   }
   
   public void writeln(String s) { write(s + "\n") ; }
   
   
   public static void main (String[] args) {
	   BasicThreadSplitLogger logger = new BasicThreadSplitLogger() ;
	   logger.linkLogFileToThisThread("logtest.txt") ;
	   logger.writeln("hello world!") ;
	   logger.closeThisThreadLogging();
    }
   
 
   
}
