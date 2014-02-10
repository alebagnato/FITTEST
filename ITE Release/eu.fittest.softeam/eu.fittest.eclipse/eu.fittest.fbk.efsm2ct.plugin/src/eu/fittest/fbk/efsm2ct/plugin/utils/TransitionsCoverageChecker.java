package eu.fittest.fbk.efsm2ct.plugin.utils;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.osgi.service.log.LogService;

import eu.fittest.fbk.efsm2ct.plugin.Activator;
import eu.fittest.fbk.efsm2ct.plugin.views.LogView;
import eu.fittest.fbk.efsm2ct.tools.evosuite.LogConsumer;
import eu.fittest.fbk.efsm2ct.tools.utils.TransitionParser;

public class TransitionsCoverageChecker implements LogConsumer {

	// private static Logger logger = Logger.getAnonymousLogger();

	private TransitionsTracker transitionsTracker;
	private StringBuilder buffer = new StringBuilder();
	private TransitionParser transitionParser = new TransitionParser();
	private boolean running;
	private int port;
	private IProgressMonitor monitor;
	private LogView logView;
	

	public TransitionsCoverageChecker(TransitionsTracker transitionsTracker, int port, IProgressMonitor monitor, LogView logView) {
		
		super();
		this.transitionsTracker = transitionsTracker;
		this.running = true;
		this.port = port;
		this.monitor = monitor;
		this.logView = logView;
		
	}

	@Override
	public void consume(char ch) {

		if (monitor.isCanceled()) {

			if (running) {

				// logger.info("search stopped by a user request.");
				
				try {
					terminate();
					running = false;
				} catch (UnknownHostException e) {
					
					Activator.getDefault().osgiLog(LogService.LOG_ERROR, "can't stop evosuite:", e);
				} catch (IOException e) {
					
					Activator.getDefault().osgiLog(LogService.LOG_WARNING, "shouldn't reach this point", e);
				}
			}

		} else {

			if (ch == '\n') { // is it platform dependent?

				String line = buffer.toString();

				if (transitionParser.match(line)) {

					boolean newCoverage = transitionsTracker.covered(transitionParser.getMutator(), transitionParser.getSourceState(), transitionParser.getDestState());

					if (newCoverage) {
						// logger.info("new transition covered: " + line + " " + transitionsTracker.getCovered() + "/" + transitionsTracker.size());
						monitor.worked(1);
						logView.scheduleTextUpdate("new transition covered: " + line + " " + transitionsTracker.getCovered() + "/" + transitionsTracker.size()+"\n");

					}

					if (transitionsTracker.size() - transitionsTracker.getCovered() == 0 && running) {

						// logger.info("coverage completed.");

						try {
							terminate();
							running = false;
						} catch (UnknownHostException e) {
							Activator.getDefault().osgiLog(LogService.LOG_ERROR, "can't stop evosuite:", e);
						} catch (IOException e) {
							Activator.getDefault().osgiLog(LogService.LOG_WARNING, "program shouldn't reach this point: " + e);
						}

					}

				}

				if (buffer.length() > 0) {
					buffer.delete(0, buffer.length());
				}

			} else {
				buffer.append(ch);
			}
		}

	}

	public void terminate() throws UnknownHostException, IOException {

		Socket s = new Socket("localhost", port); // TODO remove host ip address
		s.close();

	}

}
