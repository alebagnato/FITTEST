package eu.fittest.fbk.efsm2ct.tools.evosuite;

import java.io.InputStream;
import java.util.List;

public class ProcessMonitor implements Runnable {

	private Process process;
	private int status;
	private boolean terminated;
	private Object synObj = new Object();
	private boolean exceptionRisen;
	private Throwable exception;
	private InputStreamMonitor isMonitor;
	private InputStream inputStream;

	public ProcessMonitor(Process process) {
		this.process = process;
		this.inputStream = process.getInputStream();
		isMonitor = new InputStreamMonitor(inputStream);
	}

	@Override
	public void run() {

		new Thread(isMonitor).start();

		try {

			status = process.waitFor();		
			isMonitor.waitIsClosed();
			
			terminated = true;

		} catch (InterruptedException e) {

			exception = e;
			exceptionRisen = true;
			terminated = true;

		}
	

		synchronized (synObj) {
			synObj.notifyAll();
		}

	}

	public int getStatus() {
		return status;
	}

	public boolean isTerminated() {
		return terminated;
	}

	public boolean isExceptionRisen() {
		return exceptionRisen;
	}

	public Throwable getException() {
		return exception;
	}

	public void waitTerminated() throws InterruptedException {

		synchronized (synObj) {

			if (!terminated) {

				synObj.wait();

			}

		}

	}

	public void setLogConsumers(List<LogConsumer> logConsumers) {

		isMonitor.setLogConsumers(logConsumers);

	}

}
