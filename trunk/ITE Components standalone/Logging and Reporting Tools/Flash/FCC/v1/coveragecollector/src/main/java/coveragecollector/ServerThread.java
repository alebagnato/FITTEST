package coveragecollector;

import java.lang.StringBuilder;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.List;

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
  
public class ServerThread implements Runnable
{
    private final CoverageCollector collector;
    private ServerSocket serversocket;

    protected byte EOF = (byte) 0x00;

    public ServerThread(CoverageCollector collector, ServerSocket serversocket)
    {
        this.collector = collector;
        this.serversocket = serversocket;
    }

	private boolean isPolicyFileRequested(BufferedReader reader) throws IOException {
		System.out.println("Checking socket policy file request ...");
		String policyTag = "<policy-file-request/>";
		int policyIx = 0;
		boolean eofByteRead = false;
		int codePoint;
		do {
			codePoint = reader.read();
			if (codePoint == -1)
			{
				System.out.println(" ... policy file not requested");
				return false;
			}
			System.out.print((char)codePoint);
			if ( !((char)codePoint == policyTag.charAt(policyIx)) )
			{
				System.out.println(" ... policy file not requested");
				return false;
			}
			policyIx++;
		} while (policyIx < policyTag.length());
		reader.read(); // byte 0 (AS end-of-line)
		System.out.println(" ... socket security checked");
		return true;
    }

	private String buildPolicyFile() {
		return "<?xml version=\"1.0\" encoding=\"UTF-8\"?><cross-domain-policy xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://www.adobe.com/xml/schemas/PolicyFileSocket.xsd\"><allow-access-from domain=\"*\" to-ports=\"*\" secure=\"false\" /><site-control permitted-cross-domain-policies=\"master-only\" /></cross-domain-policy>";
	}

	private boolean checkASsecurity(Socket socket) throws IOException {
		System.out.println("Checking sockets security");
		BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream(), "UTF-8"));
		if (isPolicyFileRequested(reader)) {
			System.out.println("Requested policy file");

			OutputStream writer = socket.getOutputStream();			
			writer.write(buildPolicyFile().getBytes("UTF-8"));
			writer.write(EOF);                	
			writer.flush();
			
			socket.close();
			System.out.println("Sent policy file");
			return true;
		}
		return false;
    }	

    @Override
    public void run()
    {
        List<Socket> sockets = new ArrayList<Socket>(); 
        while (!serversocket.isClosed())
        {

            try
            {
            	String inputLine;
            	
                Socket socket = serversocket.accept();
                sockets.add(socket);
				if (checkASsecurity(socket))
				{
					sockets.remove(socket);
					socket = serversocket.accept();
					sockets.add(socket);
				}

            	System.out.println("Connection established");
            	
            	/*OutputStream writer = socket.getOutputStream();
            	String hf = "Hello Flash";
            	writer.write(hf.getBytes("UTF-8"));
            	writer.write(EOF);
            	writer.flush();*/
            	
                Thread thread = new Thread(new CoverageThread(socket, collector));
                thread.start();
            } 
            catch (SocketException e)
            {
            	System.out.println("Socket exception: " + e.getMessage());
                System.err.println(e.getMessage());
            }
            catch (IOException e)
            {
                System.out.println("IOException: " + e.getMessage());
                e.printStackTrace();
            }

        }
        for (Socket socket : sockets)
        {
            if(!socket.isClosed()) 
            {
                try
                {
            		System.out.println("Closing socket ...");
                    socket.close();
            		System.out.println(" ,,, socket closed");
                } catch (IOException e)
                {
                    e.printStackTrace();
                }
            }
        }
    }
}
