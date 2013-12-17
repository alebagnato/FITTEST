package coveragecollector;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.Socket;
import java.net.SocketException;

/**
  * Universitat Politecnica de Valencia 2013
  * Camino de Vera, s/n
  * 46022 Valencia, Spain
  * www.upv.es    
  */

/**
  * authors: Arthur Baars and Urko Rueda Molina
  * version 1.0
  * package coveragecollector
  */
  
public class CoverageThread implements Runnable
{

    private final Socket socket;
    private final CoverageCollector collector; 

    public CoverageThread(Socket socket, CoverageCollector collector)
    {
        this.socket = socket;
        this.collector = collector;
        
    }
    
    @Override
    public void run()
    {
        System.out.println("** CoverageThread: detecting a client on " + socket) ;
        InputStream input;
        try
        {
            input = socket.getInputStream();
            BufferedReader reader = new BufferedReader(new InputStreamReader(input));
            String line;
            while(!socket.isClosed())
            {
                line = null;
                try
                {
                    line = reader.readLine();
                }
                catch(SocketException e)
                {
                    System.err.println(e.getMessage());
                }
                catch(IOException e)
                {
                    e.printStackTrace();
                }
                if(line == null) 
                    break;
                else
                    collector.coverageInfo(line);
            }
            
        } catch (IOException e)
        {
            e.printStackTrace();
        }
        finally
        {
            try
            {
                System.out.println("** Closing " + socket) ;
                socket.close();
                collector.coverageThreadClosed();
            } catch (IOException e)
            {
                e.printStackTrace();
            }
        }
       
    }

}
