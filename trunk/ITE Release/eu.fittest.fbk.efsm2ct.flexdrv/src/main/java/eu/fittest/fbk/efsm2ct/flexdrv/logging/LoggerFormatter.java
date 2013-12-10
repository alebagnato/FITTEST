/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.flexdrv.logging;

/**
 *
 * @author Bno (http://stackoverflow.com/questions/194765/how-do-i-get-java-logging-output-to-appear-on-a-single-line)
 */
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

public final class LoggerFormatter extends Formatter {

	private static final String LINE_SEPARATOR = System.getProperty("line.separator");

	@Override
	public String format(LogRecord record) {

		int count = Thread.activeCount();
	   
	     Thread th[] = new Thread[count];
	     // returns the number of threads put into the array 
	     Thread.enumerate(th);
	    
	     String thrName = null;
	     
	     // prints active threads
	     for (int i = 0; i < count; i++) {
	    	 
	    	 if (th[i] != null && th[i].getId() == record.getThreadID()) {
	    		 
	    		 thrName = th[i].getName();
	    		 break;
	    		 
	    	 }
	    	 
	     }
		
		if (thrName == null) {
			thrName = String.format("id:%d", record.getThreadID());
		}
		
		StringBuilder sb = new StringBuilder();

		sb.append(new Date(record.getMillis()))
		  .append(" ")
		  .append(record.getLevel().getLocalizedName())
		  .append(" ")
		  .append("[")
		  .append(record.getSourceClassName())
		  .append("]")
		  .append("{").append(thrName).append("}: ")
		  .append(formatMessage(record)).append(LINE_SEPARATOR);

		if (record.getThrown() != null) {
			try {
				StringWriter sw = new StringWriter();
				PrintWriter pw = new PrintWriter(sw);
				record.getThrown().printStackTrace(pw);
				pw.close();
				sb.append(sw.toString());
			} catch (Exception ex) {
				// ignore
			}
		}

		return "RT: " + sb.toString();
	}
}
