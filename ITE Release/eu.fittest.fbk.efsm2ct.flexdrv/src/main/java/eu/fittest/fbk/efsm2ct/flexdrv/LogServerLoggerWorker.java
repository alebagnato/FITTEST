/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.flexdrv;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;

import eu.fittest.fbk.efsm2ct.flexdrv.logging.LoggerManager;

/**
 * 
 * @author tiella
 */
class LogServerLoggerWorker implements Runnable {

	private Socket csocket;
	private final InputStream inputStream;
	private final OutputStream outputStream;
	private boolean isStopping;
	private boolean connectionSetupCompleted;
	private Thread th;
	private BufferedReader bis;
	private LogServerLogger logServer;
	private int id;
	private boolean exited;

	public LogServerLoggerWorker(Socket csocket, LogServerLogger logServer, int id) throws IOException, JAXBException {

		this.csocket = csocket;
		this.id = id;
		this.logServer = logServer;

		inputStream = csocket.getInputStream();
		outputStream = csocket.getOutputStream();

	}

	@Override
	public void run() {

		bis = new BufferedReader(new InputStreamReader(inputStream));

		while (true) {
			try {

				debug("is reading from socket ...");

				String line = bis.readLine();

				debug("log line read:" + line);

				if (line == null) {
					// log(sb.toString());
					log("input terminated.");
					break;
				}

				logServer.receiveLine(line);

			} catch (IOException ex) {

				if (isStopping) {
					log("stop requested, exiting.");
					break;
				}

				log("exception:" + ex);
			} catch (Throwable ex) {

				log("generic exception:" + ex);
				break;
			}

		}

		try {
			csocket.close();
		} catch (IOException ex) {
			log("exception:" + ex);
		}

		exited = true;
		log.fine("exited.");
		
		
	}

	void start() {

		th = new Thread(this, "csocket-" + csocket.getRemoteSocketAddress() + "-worker");

		th.start();
	}

	void stop() {

		isStopping = true;

		try {
			bis.close();

		} catch (IOException ex) {
			// Logger.getLogger(LogServerLoggerWorker.class.getName()).log(Level.SEVERE,
			// null, ex);
			log("exception:" + ex);
		}

	}

	public boolean isConnectionCompleted() {

		return connectionSetupCompleted;

	}

	public String getSocketStatus() {

		StringBuilder sb = new StringBuilder();

		sb.append(csocket.isConnected() ? "connected" : "not connected");
		sb.append(", ");
		sb.append(csocket.isClosed() ? "closed" : "not closed");

		return sb.toString();

	}

	static final Logger log = Logger.getLogger(LoggerManager.LOGGER);

	private void log(String msg) {
		log.fine(getName() + " " + msg);
	}

	public String getName() {
		return "worker " + id + " of " + logServer.getSsocket().getLocalPort();

	}

	private void debug(String msg) {
		log.finer(msg);
	}

	public boolean isExited() {
		
		return exited;
	}
}
