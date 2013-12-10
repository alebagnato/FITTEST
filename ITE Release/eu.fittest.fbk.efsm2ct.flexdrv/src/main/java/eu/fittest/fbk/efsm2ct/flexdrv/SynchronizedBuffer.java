/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.flexdrv;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import eu.fittest.fbk.efsm2ct.flexdrv.logging.LoggerManager;

/**
 * 
 * @author tiella
 */
public class SynchronizedBuffer {
	
	public enum State {

		IDLE, RECEIVING, RECEIVED
	}
	
	static final Logger log = Logger.getLogger(LoggerManager.LOGGER);

	private boolean loggingIdleMessagesToFile = false;
	private PrintStream logStream;
	private List<LogConsumer> logConsumers = new ArrayList<LogConsumer>();
	

	private StringBuilder data;
	private int dataCounter = -2;
	private State state = State.IDLE;
	private Lock lock = new ReentrantLock();
	private Condition cond = lock.newCondition();

	private Level level = Level.FINE;
	
	
	public SynchronizedBuffer() {
		super();
		
		level=Level.parse(System.getProperty(FlexDriver.WARNINGLEVEL_PROPERTY, level.toString()));
		
	}

	private boolean isLoggingIdleMessagesToFile() {

		return loggingIdleMessagesToFile;
	}

	public void setLoggingIdleMessagesToFile(boolean loggingIdleMessagesToFile) {
		this.loggingIdleMessagesToFile = loggingIdleMessagesToFile;
		
		if (!loggingIdleMessagesToFile && logStream != null) {
			logStream.close();
			logStream = null;
		}
		
	}

	private void logToFile(String line) {

		if (logStream == null) {
			try {
				logStream = new PrintStream(File.createTempFile("flexstore", ".log"));
			} catch (IOException ex) {
				log.log(Level.SEVERE, null, ex);
			}

		}

		logStream.println(line);

	}



	public void readyToReceive() {

		log.finer("readyToReceive, locking ...");

		lock.lock();

		log.finer("readyToReceive, locked ...");

		while (state != State.IDLE) {
			try {
				log.fine("readyToReceive, waiting for state == IDLE");
				cond.await();
			} catch (InterruptedException ex) {
				log.log(Level.SEVERE, "can't wait for state == IDLE", ex);
			}
		}

		log.fine("readyToReceive, state == IDLE");

		log.fine("readyToReceive, changing state to RECEIVING");

		state = State.RECEIVING;
		dataCounter = -1;
		data = new StringBuilder();

	}

	public void addLine(String line) {

		log.finer("addLine, locking...");

		lock.lock();

		log.finer("addLine, locked");
		
		for (LogConsumer c : logConsumers) {
			c.consume(line);
		}

		switch (state) {
		case IDLE:
			
			if (isLoggingIdleMessagesToFile()) {

				logToFile(line);

			} else {
				log.log(level,"unexpected line in state [" + state + "]: " + line);
			}
			break;
		case RECEIVED:
			log.warning("unexpected line in state [" + state + "]: " + line);
			break;
		case RECEIVING:

			// while (state != State.RECEIVING) {
			// try {
			// log("addLine, waiting for RECEIVING");
			//
			// cond.await();
			// } catch (InterruptedException ex) {
			// Logger.getLogger(SynchronizedBuffer.class.getName()).log(Level.SEVERE,
			// null, ex);
			// }
			// }

			log.fine("addLine, state is RECEIVING");

			data.append(line).append('\n');

			if (dataCounter == -1) {
				dataCounter = 0;
			}

			if (line.startsWith("%<")) {
				dataCounter++;
			}

			if (line.endsWith("%>")) {
				dataCounter--;
			}

			if (dataCounter == 0) {

				log.fine("addLine, changing state to RECEIVED");
				state = State.RECEIVED;

				log.finer("addLine, signaling...");
				cond.signalAll();

			}
			break;
		}

		log.finer("addLine, unlocking...");
		lock.unlock();

	}

	public boolean waitDataReady() {

		log.finer("waitDataReady, unlocking...");

		lock.unlock();

		log.finer("waitDataReady, locking...");

		lock.lock();

		log.finer("waitDataReady, locked ...");

		while (state != State.RECEIVED) {
			try {
				log.fine("waitDataReady, waiting for RECEIVED");
				if (!cond.await(5, TimeUnit.SECONDS)) {
					return false;
				}
			} catch (InterruptedException ex) {
				log.log(Level.SEVERE, "can't wait for RECEIVED", ex);
			}

		}

		log.fine("waitDataReady, state is RECEIVED");
		return true;

	}

	public String getBufferContent() {
		return data.toString();
	}

	public void unlock() {

		log.fine("unlock. setting state to IDLE");

		state = State.IDLE;

		log.finer("unlock, signaling ...");
		cond.signal();

		log.finer("unlock, unlocking ...");
		lock.unlock();
	}

	public void addLogConsumer(LogConsumer c) {
		
		logConsumers.add(c);
		
	}

	

}
