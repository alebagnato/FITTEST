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

import eu.fittest.common.core.xml.Initialize;
import eu.fittest.common.core.xml.Start;
import eu.fittest.common.core.xml.Stop;
import eu.fittest.common.core.xml.Terminate;
import eu.fittest.fbk.efsm2ct.flexdrv.logging.LoggerManager;

/**
 * 
 * @author tiella
 */
public class LogServerCommand implements Runnable {

	private int workersId = 1;
	private ServerSocket ssocket;
	private Thread thread;
	private boolean isStopping = false;
	private boolean stopped = false;
	private boolean connectionSetupCompleted = false;
	private List<LogServerCommandWorker> logServerWorkers = new ArrayList<LogServerCommandWorker>();
	private LogServerCommandWorker responsibleWorker;
	private String sutHostAddress;
	private SocketList openSockets;
	private int dataPort;

	public LogServerCommand(SocketList openSockets, String sutHostAddress, int dataPort, int minport, int maxport) throws IOException, FlexDriverException {

		this.openSockets = openSockets;
		this.sutHostAddress = sutHostAddress;
		int port = minport;
		this.dataPort = dataPort;

		while (port < maxport) {

			try {

				this.ssocket = new ServerSocket(port);
				log.fine("server bound to port:"+port);
				openSockets.add(ssocket);
				break;

			} catch (IOException ex) {

				log("can't bind to port:" + port + ": " + ex);
				log("port " + port + " already used, trying next");
				port++;
			}
		}

		if (port == maxport) {
			throw new FlexDriverException("can't bind to any port, all address in range:" + minport + "-" + maxport + " in use.");
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

					log("accepted");
					// assert (logServerWorker == null);
					LogServerCommandWorker logServerWorker = 
							new LogServerCommandWorker(dataPort, csocket, this, workersId);

					workersId++;

					logServerWorker.start();

					logServerWorkers.add(logServerWorker);

				} catch (JAXBException ex) {

					Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
					break;

				} catch (SocketException ex) {

					if (!isStopping) {
						Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
					} else {
						log("server was stopped.");
						stopped = true;
						break;
					}
				}

			} catch (IOException ex) {
				Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
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
			Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
		}

	}

	public boolean isConnectionSetupCompleted() {

		return connectionSetupCompleted;

	}

	public void sendStartLogging() {

		Start st = new Start();

		try {

			LogServerCommandWorker w = getResponsibleWorker(); // take the
																// second one

			w.sendMessage(st);

		} catch (JAXBException ex) {
			Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
		} catch (IOException ex) {
			Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
		}

	}

	void setConnectionSetupCompleted(LogServerCommandWorker responsibleWorker) {
		connectionSetupCompleted = true;
		this.responsibleWorker = responsibleWorker;
	}

	void reportOnWorkers() {

		log("spawned workers:" + logServerWorkers.size());

		for (LogServerCommandWorker w : logServerWorkers) {
			log("socket status:" + w.getSocketStatus());
		}

	}

	void sendInitializeLogging() {

		log("sending intialize command");

		Initialize st = new Initialize();
		Initialize.Parameter p = new Initialize.Parameter();

		p.setName("loggingLevel");
		p.setValue("5");

		st.getParameter().add(p);

		try {

			LogServerCommandWorker w = getResponsibleWorker(); // take the
																// second one

			if (w != null) {
				w.sendMessage(st);
			} else {
				log("WARNING: no responsible worker found to send:" + st);
			}

		} catch (JAXBException ex) {
			Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
		} catch (IOException ex) {
			Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
		}

	}

	void sendTerminatedLogging() {
		Terminate st = new Terminate();

		try {

			LogServerCommandWorker w = getResponsibleWorker(); // take the
																// second one

			if (w != null) {
				w.sendMessage(st);
			} else {
				log("WARNING: no responsible worker found to send:" + st);
			}

		} catch (JAXBException ex) {
			Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
		} catch (IOException ex) {
			Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	static final Logger log = Logger.getLogger(LoggerManager.LOGGER);

	private void log(String msg) {
		log.fine(msg);
	}

	void sendStopLogging() {
		Stop st = new Stop();

		try {

			LogServerCommandWorker w = getResponsibleWorker(); // take the
																// second one

			if (w != null) {
				w.sendMessage(st);
			} else {
				log("WARNING: no responsible worker found to send:" + st);
			}

		} catch (JAXBException ex) {
			Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
		} catch (IOException ex) {
			Logger.getLogger(LogServerCommand.class.getName()).log(Level.SEVERE, null, ex);
		}

	}

	private LogServerCommandWorker getResponsibleWorker() {
		return responsibleWorker;
	}

	public void stopWorkers() {
		log("stopping each worker");

		for (LogServerCommandWorker w : logServerWorkers) {
			log("stopping cmd worker:" + w.getName());
			w.stop();
			log("sent, stopping cmd worker:" + w.getName());

		}
		
		boolean finished = false;
		
		while(finished) {
			
			finished = true;
			
			for (LogServerCommandWorker w : logServerWorkers) {
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
		
		// reset connection state
		responsibleWorker = null;
		connectionSetupCompleted = false;
		logServerWorkers.clear();
	}
}
