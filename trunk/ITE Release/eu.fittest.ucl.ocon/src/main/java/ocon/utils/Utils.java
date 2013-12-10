package ocon.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;

class StreamGobbler extends Thread
{
    InputStream is;
    OutputStream os;
    StringBuilder sb;
    StreamGobbler(InputStream is)
    {
        this(is, null);
    }
    StreamGobbler(InputStream is, OutputStream redirect)
    {
        this.is = is;
        this.os = redirect;
        this.sb = new StringBuilder();
    }
    public String toString()
    {
    	return this.sb.toString();
    }
    public void run()
    {
    	try
        {
            PrintWriter pw = null;
            if (os != null)
                pw = new PrintWriter(os);
                
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);
            String line=null;
            while ( (line = br.readLine()) != null)
            {
                if (pw != null)
                    pw.println(line);
                this.sb.append(line + '\n');
            }
            if (pw != null)
            {
                pw.flush();
                pw.close();
            }
            br.close();
            isr.close();
        } catch (IOException ioe)
        {
           ioe.printStackTrace();  
        }
    }
}

public class Utils {

	private static class Worker extends Thread {
		private final Process process;
		private Integer exit = null;

		private Worker(Process process) {
			this.process = process;
		}

		public void run() {
			try {
				exit = process.waitFor();
			} catch (InterruptedException ignore) {
				return;
			}
		}
	}
	
	public static String sysExec(String command, String directory, int timeout) {
		
		try {
            
			//System.out.println("Executing system command:\n"+command);

			Process process = null;
			
            if (directory == null) {
                process = Runtime.getRuntime().exec(command);
            } else {
                process = Runtime.getRuntime().exec(command,
                        null, new File(directory));
            }
            
            StreamGobbler errorGobbler = new StreamGobbler(process.getErrorStream());
            StreamGobbler outputGobbler = new StreamGobbler(process.getInputStream());
            
            errorGobbler.start();
            outputGobbler.start();

            Worker worker = new Worker(process);
            worker.start();

            String result = null;
            try {
				worker.join(timeout * 1000);
				while(worker.isAlive())
				{
					worker.interrupt();
					worker.join();
				}
				if (worker.exit != null)
					result = outputGobbler.toString();
			} catch (InterruptedException ex) {
			} finally {
				process.getInputStream().close();
            	process.getErrorStream().close();
            	process.getOutputStream().close();
				process.destroy();
			}
			
			return result;
            
        } catch (IOException e) {
            throw new RuntimeException("Error whilst executing command:\n "
                    + command + "\n" + e.getMessage());
        }
    }

	@Deprecated
	public static String sysExec(String command, String directory) {
		
		try {
            
			//System.out.println("Executing system command:\n"+command);

			Process process = null;
			StringBuilder sb = new StringBuilder();
			
            if (directory == null) {
                process = Runtime.getRuntime().exec(command);
            } else {
                process = Runtime.getRuntime().exec(command,
                        null, new File(directory));
            }

            BufferedReader input = new BufferedReader (
                    new InputStreamReader(process.getInputStream()));
            
            String line=null;
            
            while ( (line = input.readLine()) != null)
            {
                sb.append(line + '\n');    
            }
            
            input.close();
            
            if(process != null)
            {
            	process.getInputStream().close();
            	process.getErrorStream().close();
            	process.getOutputStream().close();
            }
            
            return sb.toString();
            
        } catch (IOException e) {
            throw new RuntimeException("Error whilst executing command:\n "
                    + command + "\n" + e.getMessage());
        }
    }
}
