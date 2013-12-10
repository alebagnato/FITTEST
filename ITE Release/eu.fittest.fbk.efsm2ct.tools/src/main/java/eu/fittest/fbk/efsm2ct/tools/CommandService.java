package eu.fittest.fbk.efsm2ct.tools;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import eu.fittest.fbk.efsm2ct.tools.evosuite.LogConsumer;
import eu.fittest.fbk.efsm2ct.tools.evosuite.ProcessMonitor;
import eu.fittest.fbk.efsm2ct.tools.evosuite.ProcessSpawnException;

public class CommandService {

	private List<String> cmd = new ArrayList<String>();
	private String workingDirectory = System.getProperty("user.dir");
	private List<LogConsumer> logConsumers = new ArrayList<LogConsumer>();
	
	public void addLogConsumer(LogConsumer consumer) {
		logConsumers.add(consumer);
	}

	public void removeLogConsumer(LogConsumer consumer) {
		logConsumers.remove(consumer);
	}

	public String getWorkingDirectory() {
		return workingDirectory;
	}

	public void setWorkingDirectory(String workingDirectory) {
		this.workingDirectory = workingDirectory;
	}
	
	public void addCmd(String str) {
		cmd.add(str);
	}
	
	public void init() {}
	
	public int run() throws ProcessSpawnException {

		System.err.println("executing: " + asString(cmd));

		ProcessBuilder pb = new ProcessBuilder(cmd).redirectErrorStream(true);
		pb.directory(new File(workingDirectory));

		try {

			Process p = pb.start();

			ProcessMonitor processMonitor = new ProcessMonitor(p);
			processMonitor.setLogConsumers(logConsumers);

			new Thread(processMonitor).start();

			try {
				processMonitor.waitTerminated();
				System.err.println("command terminated");
			} catch (InterruptedException e) {
				throw new ProcessSpawnException("can't run command to termination", e);
			}

			if (processMonitor.isExceptionRisen()) {
				throw new ProcessSpawnException("can't run command to termination", processMonitor.getException());
			}

			return processMonitor.getStatus();

		} catch (IOException ex) {

			throw new ProcessSpawnException("can't start command", ex);

		}

	}
	
	private String asString(List<String> cmd2) {

		StringBuilder sb = new StringBuilder();

		for (String s : cmd2) {
			sb.append(s);
			sb.append(" ");
		}

		if (sb.length() > 0) {

			sb.deleteCharAt(sb.length() - 1);

		}

		return sb.toString();
	}
	
}
