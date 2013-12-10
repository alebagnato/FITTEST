package eu.fittest.modelInference.fsmInference.modelInference;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.io.*;

import eu.fittest.modelInference.fsmInference.fsm.*;
import eu.fittest.modelInference.fsmInference.utility.Utility;

/**
 * 
 * @author Alessandro Marchetto Model inference with log written in files
 */
public class ModelInference {
	Log log = new Log();
	StatesSequence statesOfallSessions = new StatesSequence();
	// StatesSet setOfStates=new StatesSet();
	// Fsm fsm=new Fsm();

	Line line = null;
	String tmp = "org";
	int listOfFiles = 0;
	Utility utils = new Utility();

	void newLog() {
		log = new Log();
	}

	/*
	 * It parsers each file in a log
	 */
	Line processLineOfFile(String fileName, String item, boolean addToLog) {
		try {
			Pattern p = Pattern.compile("(.*):__:(.*):__:(.*)");
			Matcher matcher = p.matcher(item);

			String event;
			String state;
			String eventName;
			String[] status;

			if (matcher.matches()) {
				event = matcher.group(1);
				eventName = matcher.group(2);
				state = matcher.group(3);
				state = state.substring(1, state.length() - 1);
				status = state.split("\\s*;__;\\s*");
				line = new Line(fileName, event, eventName, status);
				if (addToLog) {
					log.addLine(line);
					return line;
				} else
					return line;
			}
			return null;
		} catch (Exception e) {
			return null;
		}
	}

	public void getSizeOfListOfFiles(String folderPath) {
		try {
			boolean exists = (new File(folderPath)).exists();

			if (exists) {
				File folder = new File(folderPath);
				File[] listOfFiles = folder.listFiles();
				this.listOfFiles = listOfFiles.length;
			}
		} catch (Exception e) {
			System.out
					.println("Error in fit1ModelUpdate.ModelInference.inference.ModelInference.getListOfFiles");
		}
	}

	/*
	 * It infers a set of states from logs
	 */
	public Fsm statesInference(File[] listOfFiles, int execIndex) {
		try {

			boolean addEndState = false;
			boolean addStartState = false;
			String fname = "";
			BufferedReader inputFile = null;
			Line lastLine = null;

			for (int j = 0; j < listOfFiles.length; j++) {

				// System.out.println(j);

				if (listOfFiles[j].isFile()) {
					fname = listOfFiles[j].getAbsolutePath();

					// if
					// ((listOfFiles[j].getName().startsWith("log_"))&&(listOfFiles[j].getName().endsWith(".txt")))
					// {
					if (listOfFiles[j].getName().startsWith("log_")) {

						addEndState = false;
						addStartState = false;

						inputFile = new BufferedReader(new FileReader(fname));

						String str;
						newLog();

						lastLine = null;

						while ((str = inputFile.readLine()) != null) {
							lastLine = processLineOfFile(fname, str, true);
							addStartState = true;
							addEndState = true;
						}

						inputFile.close();

						if (log.size() > 0) {
							if (j == 0)
								statesOfallSessions.apply(true, fname, log,
										addStartState, addEndState, lastLine,
										execIndex);
							else
								statesOfallSessions.apply(false, fname, log,
										addStartState, addEndState, lastLine,
										execIndex);

							// setOfStates.addStates(statesOfallSessions.stateSet);
							// setOfStates.addToCountEventsInLogs(log.size());

						}

					}
				}
			}

			return statesOfallSessions.stateSet;

		} catch (Exception e) {
			System.out
					.println("Error in fit1ModelUpdate.ModelInference.inference.ModelInference.statesInference");
			e.printStackTrace();
			return null;

		}
	}

}
