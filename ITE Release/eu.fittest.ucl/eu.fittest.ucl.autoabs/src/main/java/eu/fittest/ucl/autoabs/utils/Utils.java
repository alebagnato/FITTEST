/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */

package eu.fittest.ucl.autoabs.utils;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Logger;

import au.com.bytecode.opencsv.CSVReader;

import eu.fbk.se.fsm.FSM;
import eu.fittest.ucl.autoabs.cluster.Cluster;
import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;
import eu.fittest.ucl.autoabs.evolve.Chromosome;
import eu.fittest.ucl.autoabs.evolve.Fitness;
import eu.fittest.ucl.autoabs.evolve.MultiObjectiveFitness;

/**
 * Utility class for parsing a CSV file (which denotes an execution trace)
 * and converting each line into a ConcreteState states
 *
 */
public class Utils {
	
	public static final String eol = System.getProperty("line.separator");
	public static Logger logger = null;
	
	private static LinkedHashSet<ConcreteState> concreteStates = new LinkedHashSet<ConcreteState>();
		
	/** This function clears the set of stored, unique concrete states
	 * that have been parsed from CSV file(s)
	 */
	public static void clearConcreteStateSet() {
		concreteStates.clear();
	}
	
	/** This function parses a csv file, constructing a trace event for each row
	 *  The function also extracts concrete state information and stores it
	 * @param csvFiles A list of csv files to parse
	 */
	public static Vector<List<TraceEvent>> computeAllTraceEvents(List<String> csvFiles) {
		Vector<List<TraceEvent>> events = new Vector<List<TraceEvent>>();
		for(String f : csvFiles) {
			for(List<TraceEvent> trace : parseCSVFile(f, true, '"')) {
				events.add(trace);
			}
		}
		return events;
	}
	
	/** This function returns a set of unique states to be used for clustering
	 *  
	 * @return A set of unique concrete states, parsed from
	 * CSV file(s)
	 */
	public static LinkedHashSet<ConcreteState> getConcreteStates() {
		return concreteStates;
	}
	
	/**
	 * Function to execute an external process
	 * @param command Command and paremters to execute
	 * @param dir Directory path in which to execute the command
	 * @return Success or Failure (i.e. exception thrown) when executing command
	 */
	public static boolean executeProcess(String[] command, String dir) {
		Process process = null;
		boolean success = true;
		try{
			if(dir == null)
				process = Runtime.getRuntime().exec(command, null, null);
			else
				process = Runtime.getRuntime().exec(command, null, new File(dir));
			//StreamGobbler errorGobbler = new StreamGobbler(process.getErrorStream());
			//errorGobbler.start();
			try {
				success = process.waitFor() == 0;
			} catch (InterruptedException e) {
				e.printStackTrace();
				success = false;
			}
			if(!success) {
				//System.out.println(errorGobbler.toString());
				if(logger != null)
					logger.info("could not wait for process");
			}
		} catch (IOException e) {
			e.printStackTrace();
			success = false;
		} finally {
			if(process != null) {
				/*try{
					process.getInputStream().close();
	            	process.getErrorStream().close();
	            	process.getOutputStream().close();
				} catch (IOException e){
				}*/
				process.destroy();
				process = null;
			}
		}
		
		return success;
	}
	
	/** This function parses a CSV file and converts it into a list of separate trace events
	 * Each list denotes a complete trace and different traces must be separated by a START event line
	 * @param filename The CSV filename
	 * @param createNewStates A flag whether to allow creation of new states for csv entries, or whether the
	 * parsed state must match a previously parsed state (i.e. in concreteStates list)
	 * @return A vector of TraceEvent lists, each list denotes a unique execution trace
	 */
	public static Vector<List<TraceEvent>> parseCSVFile(String filename, boolean createNewStates, char quoteChar) {
		CSVReader reader = null;
		boolean parsedHeader = false;
		
		String [] nextLine;
		int valueCount = 0;
		Object[] values = null;
		int index = 0;
		ConcreteState state = null;
		Vector<List<TraceEvent>> events = new Vector<List<TraceEvent>>();
		List<TraceEvent> trace = null;
		String sval = null;
		try {
			reader = new CSVReader(new FileReader(filename), ',', quoteChar);
			while ((nextLine = reader.readNext()) != null) {
		        // nextLine[] is an array of values from the line
				if(!parsedHeader) {
					valueCount = nextLine.length - 1; //this assumes the first column is the event name
					StringBuilder sb = new StringBuilder(valueCount);
		    		boolean prependComma = false;
		    		for(String s : nextLine) {
		    			if(prependComma)
		    				sb.append(",");
		    			sb.append(s);
		    			prependComma = true;
		    		}
		    		TraceEvent.setTraceHeader(sb.toString());
		    		parsedHeader = true;
		    		trace = new ArrayList<TraceEvent>();
				} else {
					boolean isStart = true;
					index = 0;
					for(int i = 1; i < nextLine.length; i++) {
						nextLine[i] = nextLine[i].trim();
						BigDecimal _d = null;
						if((_d = Utils.isDouble(nextLine[i])) != null) {
							isStart = false;
							values[index++] = _d;//Double.parseDouble(nextLine[i]);
						} else {
							sval = nextLine[i];
							if(!sval.equals("?"))
								isStart = false;
							/*if(!sval.equals("?"))
								isStart = false;
							if(sval.isEmpty() || sval.equals("''"))
								values[index++] = "''";
							else {
								if(!sval.startsWith("\""))
									sval = "\"" + sval;
								if(!sval.endsWith("\""))
									sval = sval + "\"";
								
								values[index++] = sval;
							}*/
							if((sval.startsWith("'") && sval.endsWith("'")) ||
									(sval.startsWith("\"") && sval.endsWith("\"")))
								sval = sval.substring(1, sval.length() - 1);
							values[index++] = sval;
						}
					}
					if(isStart) {
						//save old trace first
		    			if(trace != null && trace.size() > 0) {
		    				events.add(trace);
		    			} 
		    			trace = new ArrayList<TraceEvent>();
					}
	    			//if the start event only has ? as values, then ignore it,
	    			//otherwise treat it as a real event
					if(!isStart || (isStart && values.length > 0 && !(values[0] instanceof String && values[0].equals("?")))) {
						state = getExistingState(values, !createNewStates);
						trace.add(new TraceEvent(nextLine[0], state));
					}
				}
				values = new Object[valueCount];
		    }
		} catch (FileNotFoundException e) {
			if(logger != null)
				logger.info(e.getMessage());
		} catch (IOException e) {
			if(logger != null)
				logger.info(e.getMessage());
		} finally {
			if(reader != null) {
				try {
					reader.close();
				} catch (IOException e) {
					if(logger != null)
						logger.info(e.getMessage());
				}
			}
		}
		if(trace != null && !trace.isEmpty())
			events.add(trace);
		return events;
	}	
	
	/** This function parses the WEKA clusters
	 *  and returns list of ucl.cluster.Cluster objects
	 * @throws IOException */
	public static List<Cluster> parseWekaClusters(String clusterDir) throws IOException{
		File folder = new File(clusterDir);
		File[] listOfFiles = folder.listFiles();
		folder = null;
		String filename = null;
		//String csvName = null;
		List<Cluster> clusters = new ArrayList<Cluster>();
		for (int i = 0; i < listOfFiles.length; i++) {
			filename = listOfFiles[i].getCanonicalPath();
			if(filename.endsWith(".csv")) {
				Vector<List<TraceEvent>> events = parseCSVFile(filename, false, '\'');
				//extract all the concrete states and generate clusters
				Cluster c = new Cluster();
				//I know events only contains 1 list
				for(TraceEvent event : events.get(0)) {
					c.addState(event.getConcreteState());
				}
				clusters.add(c);
			}
		}
		
		return clusters;
	}
	
	/** This method writes all the unique states to a csv file
	 * @param header The csv header
	 * @return A File object to the csv file
	 */
	/*public static File saveUniqueSatesToCSV(File base, String header) {
		File csv = new File(base, "unqiue.csv");
		if(csv.exists())
			csv.delete();
		BufferedWriter bw = null;
		boolean writeHeader = true;
		
		try {
			bw = new BufferedWriter(new FileWriter(csv));
		} catch (IOException e1) {
			if(logger != null)
				logger.info(e1.getMessage());
		}
		
		for(ConcreteState state : concreteStates) {
			try {
				if(writeHeader) {
					bw.write(header + '\n');
					writeHeader = false;
				}
				bw.write("dummy," + state.toString() + '\n');
			} catch (IOException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
		try {
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return csv;
	}*/
	
	public static Collection<File> saveClustersForDaikon(
			File clusterDir, Vector<List<TraceEvent>> events, 
			Collection<Cluster> clusters) {
		
		if(!clusterDir.exists())
			clusterDir.mkdirs();
		else
			emptyDir(clusterDir, false);
		
		String csvHeader = TraceEvent.getTraceHeader();
		/*if(csvHeader.toLowerCase().startsWith("event,")) {
			csvHeader = csvHeader.substring(6);
		}*/
		//strip first column from CSV header, because that denotes an event name
		csvHeader = csvHeader.substring(csvHeader.indexOf(",") + 1);
		
		Map<ConcreteState, Integer> repetitionMap = new HashMap<ConcreteState, Integer>();
		ConcreteState s;
		for(List<TraceEvent> eventList : events) {
			for(TraceEvent event : eventList) {
				s = event.getConcreteState();
				Integer occurrences = repetitionMap.get(s);
				if(occurrences == null) {
					repetitionMap.put(s, 1);
				} else 
					repetitionMap.put(s, occurrences.intValue() + 1); 
			}
		}
		List<File> files = new LinkedList<File>();
		for(Cluster c : clusters) {
			int id = c.getID();
			File f = new File(clusterDir, "cluster_" + String.valueOf(id) + ".csv");
			FileWriter writer = null;
			boolean writeHeader = true;
			try{
				writer = new FileWriter(f);
				for(ConcreteState state : c.getStates()) {
					if(writeHeader) {
						writer.write(csvHeader + Utils.eol);
					}
					Integer repetitions = repetitionMap.get(state);
					for(int i = 0; i < repetitions.intValue(); i++)
						writer.write(state.toString() + Utils.eol);
					writeHeader = false;
				}
				writer.flush();
				writer.close();
			}catch(IOException e) {
				if(logger != null)
					logger.info(e.getMessage());
				return null;
			}
			files.add(f);
		}
		repetitionMap.clear();
		repetitionMap = null;
		return files;
	}
	/*
	public static void setupOutputDir(File base) {
		File clusters = new File(base, "clusters");
		try{
			workingDir = clusters.getCanonicalPath();
		} catch(IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
	}*/
	
	public static void emptyDir(File dir, boolean delete) {
		if(dir != null){
			if(dir.exists()) {
				if(dir.isDirectory()) {
					File[] files = dir.listFiles();
					for(File f : files) {
						emptyDir(f, true);
					}
				}
				if(delete)
					dir.delete();
			}
		}
	}
	public static void emptyDir(File dir) {
		emptyDir(dir, true);
	}
	
	/** This function returns an existing state if possible. States are compared by value and 
	 * may come from different csv files and lines in csv files. 
	 * @param values The object values of the state
	 * @param mustExist Throw an exception if the state object was not found in a set of existing states
	 * @return A new state if it was not found in a set of existing parsed states, or a parsed state
	 * if the state is identical to a previously observed state
	 */
	public static ConcreteState getExistingState(Object[] values, boolean mustExist) {
		ConcreteState s = new ConcreteState(values);
		return getExistingState(s, mustExist);
	}

	/**  This function returns an existing state if possible. States are compared by value and 
	 * may come from different csv files and lines in csv files. 
	 * @param state A new state object which we want to check
	 * @param mustExist Throw an exception if the state object was not found in a set of existing states
	 * @return An existing state, or the state object if it was not found
	 */
	public static ConcreteState getExistingState(ConcreteState state, boolean mustExist) {
		for(ConcreteState s : concreteStates) {
			if(areEqualStates(s, state)) {
				return s;
			}
		}
		if(mustExist) {
			throw new IllegalArgumentException("state could not be found: " + state);
		} else
			concreteStates.add(state);
			return state;
	}
	
	/** Compare two concrete states
	 * @param s1
	 * @param s2
	 * @return true if the states are equal (value wise)
	 */
	public static boolean areEqualStates(ConcreteState s1, ConcreteState s2) {
		Object[] o1 = s1.getValues();
		Object[] o2 = s2.getValues();
		if(o1.length == o2.length) {
			for(int i = 0; i < o1.length; i++) {
				if(o1[i] instanceof Double)
					o1[i] = new BigDecimal((Double)o1[i]);
				if(o2[i] instanceof Double)
					o2[i] = new BigDecimal((Double)o2[i]);
				
				if(o1[i] instanceof String && o2[i] instanceof String) {
					String str1 = (String)o1[i];
					String str2 = (String)o2[i];
					if(!str1.equals(str2))
						return false;
				} else if(o1[i] instanceof BigDecimal && o2[i] instanceof BigDecimal){
					BigDecimal d1 = (BigDecimal)o1[i];
					BigDecimal d2 = (BigDecimal)o2[i];
					if(!d1.equals(d2))
						return false;
				} else
					return false;
			}
			return true;
		}
		return false;
	}
	/** Function that returns true if I consider a csv columsn to be of type double or string
	 * 
	 * @param s The string value to check
	 * @return A boolean true if the string s represents a double and false otherwise
	 */
	public static BigDecimal isDouble(String s) {
		//boolean isDouble = false;
		if(s.isEmpty() || s.equals("''") || s.equals("\"\""))
			return null;
		if(s.startsWith("'") && s.endsWith("'"))
			s = s.substring(1, s.length() - 1);
		if(s.equals("null"))
			return null;
		BigDecimal d = null;
		try {
			d = new BigDecimal(s);
			d = d.setScale(4, RoundingMode.HALF_DOWN);
			//Double.parseDouble(s);
			//isDouble = true;
		} catch(NumberFormatException e) {
			d = null;
		}
		
		return d;
	}
	
	/** This method writes all the unique states to a csv file
	 * @param header The csv header
	 * @return A File object to the csv file
	 */
	public static File saveUniqueSatesToCSV(File outDir, String header) {
		File csv = new File(outDir, "unqiue.csv");
		if(csv.exists())
			csv.delete();
		BufferedWriter bw = null;
		boolean writeHeader = true;
		
		try {
			bw = new BufferedWriter(new FileWriter(csv));
		} catch (IOException e1) {
			e1.printStackTrace();
			System.exit(1);
		}
		
		for(ConcreteState state : concreteStates) {
			try {
				if(writeHeader) {
					bw.write(header + '\n');
					writeHeader = false;
				}
				bw.write("dummy," + state.toString() + '\n');
			} catch (IOException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
		try {
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return csv;
	}
	
	public static void saveArchiveMetrics(
			File outputDir, 
			ArrayList<Chromosome> pop, 
			MultiObjectiveFitness ff) {
		BufferedWriter bw = null;
		try {
			bw = new BufferedWriter(new FileWriter(new File(outputDir, "metrics.csv")));
			bw.write("size,infeas,nondeterm\n"); 
			for(Chromosome c : pop) {
				//save metrics to file
				FSM _fsm = c.getFSM();
				bw.write(String.valueOf(c.getFSMSize())  + "," + 
						String.valueOf(ff.countInfeasibleSequences(_fsm)) + "," +
						String.valueOf(ff.countNonDeterministicEvents(_fsm)) + "\n");
			}
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
	}
	
	public static void saveFSMPopulation(
			File outputDir,
			ArrayList<Chromosome> pop, 
			Fitness ff) {
		int counter = 0;
		PrintStream s = null;
		
		for(Chromosome c : pop) {
			if(c.getFSM() == null) ff.evaluate(c, true);
			try {
				String fsmName = "fsm_" + String.valueOf(counter) + ".fsm";
				//writer = new BufferedWriter(new FileWriter());
				s = new PrintStream(new File(outputDir, fsmName));
				c.getFSM().regenerateLabels();
				c.getFSM().print(s);
			} catch (IOException e) {
				e.printStackTrace();
			} finally {
				if(s != null) {
					s.flush();
					s.close();
				}
			}
			counter++;
		}
	}
}
