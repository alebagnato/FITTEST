/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.flexdrv;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;

import eu.fittest.fbk.efsm2ct.flexdrv.logging.LoggerManager;

/**
 * 
 * @author tiella
 */
public class LogServerLogger implements Runnable {

	private int workersId = 1;
	private ServerSocket ssocket;
	private Thread thread;
	private boolean isStopping = false;
	private boolean stopped = false;
	private List<LogServerLoggerWorker> logServerWorkers = new ArrayList<LogServerLoggerWorker>();
	private SynchronizedBuffer data = new SynchronizedBuffer();
	private SocketList openSockets;

	public LogServerLogger(SocketList openSockets, int minport, int maxport) throws IOException, FlexDriverException {

		this.openSockets = openSockets;
		int port = minport;

		while (port < maxport) {

			try {

				this.ssocket = new ServerSocket(port);
				openSockets.add(ssocket);
				log.fine("Log Server bound to port:" + port);
				break;

			} catch (IOException ex) {

				log("can't bind to port:" + port + ": " + ex);
				log("port " + port + " already used, trying next");
				port++;
			}
		}

		if (port == maxport) {
			throw new FlexDriverException("can't bind, all address in range:" + minport + "-" + maxport + " in use.");
		}

	}

	public ServerSocket getSsocket() {
		return ssocket;
	}

	public void setSsocket(ServerSocket ssocket) {
		this.ssocket = ssocket;
	}

	public Thread getThread() {
		return thread;
	}

	public void setThread(Thread thread) {
		this.thread = thread;
	}

	void start() {

		// resetData();

		Thread th = new Thread(this, "sserver-" + ssocket.getLocalPort());

		setThread(th);

		th.start();

	}

	@Override
	public void run() {

		while (true) {
			try {

				log("accepting on port:" + ssocket.getLocalPort());

				try {

					Socket csocket = ssocket.accept();
					openSockets.add(csocket);

					log("accepted:"+csocket);
					// assert (logServerWorker == null);
					LogServerLoggerWorker logServerWorker = new LogServerLoggerWorker(csocket, this, workersId);

					workersId++;

					logServerWorker.start();

					logServerWorkers.add(logServerWorker);

				} catch (JAXBException ex) {

					log.log(Level.SEVERE, null, ex);
					break;

				} catch (SocketException ex) {

					if (!isStopping) {
						log.log(Level.SEVERE, null, ex);
					} else {
						log("server was stopped.");
						stopped = true;
						break;
					}
				}

			} catch (Exception ex) {
				log.log(Level.SEVERE, null, ex);
			}

		}
	}

	public void stop() {
		try {
			workersId = 1;

			isStopping = true;

			stopWorkers();

			ssocket.close();
			
			log("waiting main listening thread to stop");
            
			while (!stopped) {
				try {
					Thread.sleep(100);
				} catch (InterruptedException e) {
					log.log(Level.SEVERE, "can't wait", e);
				}
			}
                        
            log("main listening thread stopped");
			
		} catch (IOException ex) {
			log.log(Level.SEVERE, null, ex);
		}

	}

	void reportOnWorkers() {

		log("spawned workers:" + logServerWorkers.size());

		for (LogServerLoggerWorker w : logServerWorkers) {
			log("socket status:" + w.getSocketStatus());
		}

	}

	static final Logger log = Logger.getLogger(LoggerManager.LOGGER);

	private void log(String msg) {
		log.fine(msg);
	}

	public void receiveLine(String line) {

		data.addLine(line);

	}

	public void readyToSend() {

		data.readyToReceive();

	}

	public boolean waitDataAvailableAndLock() {

		return data.waitDataReady();

	}

	public SynchronizedBuffer getData() {
		return data;
	}

	public void unlockData() {

		data.unlock();

	}

	public void setLoggingIdleMessagesToFile(boolean value) {

		data.setLoggingIdleMessagesToFile(value);

	}

	public void stopWorkers() {
		log("stopping each worker");

		for (LogServerLoggerWorker w : logServerWorkers) {

			log("stopping data worker " + w.getName());

			w.stop();

		}

		boolean finished = false;

		while (finished) {

			finished = true;

			for (LogServerLoggerWorker w : logServerWorkers) {
				log("waiting worker to exit:" + w.getName()+" "+w.isExited());
				
				if (!w.isExited()) {
					finished = false;
					try {
						Thread.sleep(100);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					break;
				}

			}
		}
		
		log("all workers exited");

		logServerWorkers.clear();

	}

	public void addLogConsumer(LogConsumer c) {
		
		data.addLogConsumer(c); 
		
	}
}
