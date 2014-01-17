package eu.fittest.fbk.efsm2ct.tools.evosuite;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.List;

public class InputStreamMonitor implements Runnable {

	private BufferedInputStream bis;
	private int port;
	private List<LogConsumer> logConsumers;
	private boolean checking = true;
	private boolean closed = false;
	private Object synObj = new Object();

	public InputStreamMonitor(InputStream is) {

		bis = new BufferedInputStream(is);

	}



	@Override
	public void run() {

		try {

			for (int ch = bis.read(); ch >= 0; ch = bis.read()) {

				// System.out.print((char) ch);

				for (LogConsumer lc : logConsumers) {
					lc.consume((char) ch);
				}

			}

			System.out.println("InputStreamMonitor: input stream closed.");

		} catch (IOException ex) {
			System.err.println("can't read" + ex);
		}
		
		closed = true;
		
		synchronized (synObj) {
			synObj.notifyAll();
		}
		
	}

	public void waitIsClosed() {
		
		synchronized (synObj) {
			
			while(!closed) {
				
				try {
					synObj.wait();
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
			}
			
		}
		
	}


	public void setLogConsumers(List<LogConsumer> logConsumers) {

		this.logConsumers = logConsumers;

	}

}
