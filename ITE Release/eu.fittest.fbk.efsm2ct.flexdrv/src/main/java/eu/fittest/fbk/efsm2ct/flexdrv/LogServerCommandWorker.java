/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.flexdrv;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.Socket;
import java.net.SocketException;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.xml.CrossDomainPolicy;
import eu.fittest.common.core.xml.PolicyFileRequest;
import eu.fittest.common.core.xml.Register;
import eu.fittest.common.core.xml.RegisterResponse;
import eu.fittest.common.core.xml.StartSerialization;
import eu.fittest.common.core.xml.StartSerializationResponse;
import eu.fittest.fbk.efsm2ct.flexdrv.logging.LoggerManager;

/**
 * 
 * @author tiella
 */
class LogServerCommandWorker implements Runnable {

	private Socket csocket;
	private final InputStream inputStream;
	private final OutputStream outputStream;
	private final JAXBContext jc;
	private final Marshaller m;
	private final Unmarshaller u;
	private boolean isStopping;
	private boolean connectionSetupCompleted;
	private Thread th;
	private InputStreamReader bis;
	private LogServerCommand logServer;
	private int id;
	private int dataPort;
	private boolean exited;

	public LogServerCommandWorker(int dataPort, Socket csocket, LogServerCommand logServer, int id) throws IOException, JAXBException {

		this.dataPort = dataPort;
		this.csocket = csocket;
		this.id = id;
		this.logServer = logServer;

		inputStream = csocket.getInputStream();
		outputStream = csocket.getOutputStream();

		jc = JAXBContext.newInstance("eu.fittest.common.core.xml");
		m = jc.createMarshaller();
		u = jc.createUnmarshaller();

	}

	@Override
	public void run() {

		bis = new InputStreamReader(inputStream);

		while (true) {
			try {

				log("is reading from socket ...");

				int ch = -2;

				StringBuilder sb = new StringBuilder();

				while (ch != 0) {

					ch = bis.read();

					if (ch > 0) {
						sb.append((char) (0x0ff & ch));
					}
				}

				if (ch == 0) {
					log("received: " + sb);

					respond(sb.toString());

				}

				if (ch == -1) {

					log("no more input, exiting.");
					break;
				}

			} catch (SocketException ex) {
				log("exception:" + ex + " is considered FATAL, exiting the receiving loop");

				break;

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

		log("exited.");
		exited = true;
		
	}

	void start() {

		th = new Thread(this, "csocket-" + csocket.getRemoteSocketAddress() + "-worker");

		th.start();
	}

	private void respond(String reqStr) throws IOException {
		try {

			Object req = u.unmarshal(new StringReader(reqStr));
			boolean connectionCompleted = false;

			Object ans = null;

			if (req instanceof PolicyFileRequest) {

				CrossDomainPolicy policy = FITTESTSingleton.getObjectFactory().createCrossDomainPolicy();
				policy.setAllowAccessFrom(FITTESTSingleton.getObjectFactory().createCrossDomainPolicyAllowAccessFrom());
				policy.getAllowAccessFrom().setDomain("*");
				policy.getAllowAccessFrom().setToPorts("*");

				ans = policy;

			} else if (req instanceof Register) {

				RegisterResponse rr = new RegisterResponse();

				rr.setFittestAgentId("ida");
				rr.setFittestComponentDir("/tmp/cx");
				rr.setFittestComponentId("cx");
				rr.setFittestIteId("itelite");

				ans = rr;

				// answering to a register message implies that the connection
				// is completed.
				connectionCompleted = true;

			} else if (req instanceof StartSerialization) {

				StartSerializationResponse ssr = new StartSerializationResponse();

				ssr.setAddress("localhost" + ":" + dataPort);

				ans = ssr;

			}

			if (ans != null) {

				log("responding ...");
				sendMessage(ans);

				if (connectionCompleted) {
					logServer.setConnectionSetupCompleted(this);
				}

			} else {
				// Logger.getLogger(LogServerCommandWorker.class.getName()).log(Level.SEVERE,
				// "unknown request");
				// Logger.getLogger(LogServerCommandWorker.class.getName()).log(Level.SEVERE,
				// reqStr);
				log("exception unknown request: " + reqStr);

			}

		} catch (JAXBException ex) {
			// Logger.getLogger(LogServerCommandWorker.class.getName()).log(Level.SEVERE,
			// null, ex);
			log("exception:" + ex);
		}

	}

	private void sendString(String str) throws IOException {

		outputStream.write(str.getBytes());
		outputStream.write(0);
		outputStream.flush();

	}

	void stop() {

		isStopping = true;

		try {
			bis.close();

		} catch (IOException ex) {
			// Logger.getLogger(LogServerCommandWorker.class.getName()).log(Level.SEVERE,
			// null, ex);
			log("exception:" + ex);
		}

	}

	public boolean isConnectionCompleted() {

		return connectionSetupCompleted;

	}

	public void sendMessage(Object ans) throws JAXBException, IOException {

		StringWriter sw = new StringWriter();

		m.marshal(ans, sw);

		String msg = sw.toString();

		log("sending:" + msg);

		sendString(msg);
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
		log.fine(getName() + ": " + msg);
	}

	public String getName() {
		return "worker " + id + " of " + logServer.getSsocket().getLocalPort();
	}

	public boolean isExited() {
		
		return exited;
	}
}
