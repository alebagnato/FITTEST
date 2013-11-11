package eu.fittest.ucl.api;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Random;
import java.util.logging.Logger;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import eu.fbk.se.fsm.FSM;
import eu.fbk.xinputmining.XMLUtils;
import eu.fittest.itelog.Body;

public abstract class ModelInference {
	
	private static final String EOL = System.getProperty("line.separator");
	
	protected final Random random;
	protected ArrayList<ModelInferenceListener> listeners;
	protected File temporaryOutDir;
	
	protected static final String TEMP_DIR_NAME = "out";
		
	protected Logger logger;
	
	public ModelInference(Random rand) {
		this.random = rand;
		this.listeners = new ArrayList<ModelInferenceListener>();
		this.temporaryOutDir = new File(System.getProperty("user.dir") + File.separator + TEMP_DIR_NAME);
		this.logger = Logger.getLogger("ModelInference");
	}
	public ModelInference() {
		this(new Random());
	}
	
	protected void log(String msg) {
		if(this.logger != null)
			this.logger.info(msg);
	}
	
	protected void stop() {
		for(ModelInferenceListener listener : this.listeners) {
			listener.onStop();
		}
	}
	
	private static void emptyDir(File dir) {
		if(dir != null){
			if(dir.exists()) {
				if(dir.isDirectory()) {
					File[] files = dir.listFiles();
					for(File f : files) {
						emptyDir(f);
					}
				}
				dir.delete();
			}
		}
	}
	
	protected void start() {
		for(ModelInferenceListener listener : this.listeners) {
			listener.onStart();
		}
		//empty outputdir
		emptyDir(temporaryOutDir);
	}
	
	private String getAttribute(Node n, String attr) {
		NamedNodeMap attributes = n.getAttributes();
		Node attributeNode = attributes.getNamedItem(attr);
		if(attributeNode != null) {
			return attributeNode.getNodeValue();
		}
		return "";
	}
	private void getStateValues(Node root, String prefix, 
			HashMap<String, String> values) {
		NodeList children = root.getChildNodes();
		int numChildren = children.getLength();
		for(int i = 0; i < numChildren; i++) {
			Node n = children.item(i);
			if(n.getNodeName().equals("fd")) {
				String _nodeName = this.getAttribute(n, "n");
				String fieldName = prefix + ((_nodeName.equals("elem"))? "elem" + i:_nodeName);
				Node v = n.getFirstChild();
				if(v.getNodeName().equals("V")) {
					//value
					String sty = this.getAttribute(v, "ty").toLowerCase();
					if(sty.equals("id"))
						continue;
					else if(sty.equals("int") || sty.equals("float") || sty.equals("string")) {
						String value = this.getAttribute(v, "v");
						values.put(fieldName, value);
					}
				} else {
					getStateValues(v, fieldName + ".", values);
				}
			}
		}
	}
	
	private static String quoteString(String str){
		return "\"" + str.replace("\\", "\\\\").replace("\0", "\\0").replace("\b","\\b").replace("\f","\\f").
				replace("\n","\\n").replace("\r", "\\r").replace("\t", "\\t").
				replace("\\x0B", "\\\\x0B").replace("\"", "\\\"") + "\"";
	}
	
	private void writeCsvLine(String name, LinkedHashMap<String, String> stateVariables, 
			HashMap<String, String> items, BufferedWriter bw, boolean writeStates) throws IOException {
		
		bw.write(name);
		
		Iterator<Entry<String, String>> it = stateVariables.entrySet().iterator();
		if(items == null) {
			while(it.hasNext()) {
				Entry<String, String> header = it.next();
				bw.write("," + header.getKey());
			}
		} else if(writeStates) {
			while(it.hasNext()) {
				Entry<String, String> header = it.next();
				if(items.containsKey(header.getKey())) {
					String value = items.get(header.getKey());
					if(value.indexOf(",") != -1 && !(value.startsWith("\"") && value.endsWith("\""))){
						value = quoteString(value);
					}
					bw.write("," + value);
				} else {
					String valueType = header.getValue();
					if(valueType.equals("string"))
						bw.write(",\"\"");
					else
						bw.write(",0");
				}
			}
		}
		
		bw.write(EOL);
	}
	
	/*protected List<String> convertLogToCSV(List<Body> traceFiles) throws IOException {
		return this.convertLogToCSV(traceFiles, true, false);
	}*/
	
	/**
	 * This function parses the entire log file to find the maximum number of columns the CSV file should contain.
	 * This is required to handle variable length arrays (e.g. empty shopping cart, cart with some items) etc.
	 */
	private void getAllHeaderColumns(Node root, String prefix, HashMap<String, String> stateVariables) {
		NodeList children = root.getChildNodes();
		int numChildren = children.getLength();
		for(int i = 0; i < numChildren; i++) {
			Node n = children.item(i);
			if(n.getNodeName().equals("fd")) {
				String _nodeName = this.getAttribute(n, "n");
				String fieldName = prefix + ((_nodeName.equals("elem"))? "elem" + i:_nodeName);
				Node v = n.getFirstChild();
				if(v.getNodeName().equals("V")) {
					//value
					String sty = this.getAttribute(v, "ty").toLowerCase();
					if(sty.equals("id"))
						continue;
					else if(sty.equals("int") || sty.equals("float") || sty.equals("string")) {
						if(!stateVariables.containsKey(fieldName))
							stateVariables.put(fieldName, sty);
					}
				} else {
					getAllHeaderColumns(v, fieldName + ".", stateVariables);
				}
			}
		}
	}
	
	private void analyseStateVariables(NodeList events, LinkedHashMap<String, String> stateVariables) {
		int numEvents = events.getLength();
		for(int i = 0; i < numEvents; i++) {
			NodeList eventObjects = events.item(i).getChildNodes();
			Node stateNode = eventObjects.item(1);
			this.getAllHeaderColumns(stateNode, "", stateVariables);
		}
		//stateVariables contains all variable names and their types.
	}
	
	protected List<String> convertLogToCSV(List<Body> traceFiles, 
			boolean writeStates, boolean includeTargetID) throws IOException {
		List<String> csvTraces = new ArrayList<String>();
		
		LinkedHashMap<String, String> stateVariables = new LinkedHashMap<String, String>();
		
		for (Body body : traceFiles) {
			Document doc = XMLUtils.convertToDOM(body);
			NodeList events = doc.getElementsByTagName("E");
			
			analyseStateVariables(events, stateVariables);
		}
		
		
		int counter = 1;
		for (Body body : traceFiles) {
			boolean setHeader = true;
			File traceFile = new File(this.temporaryOutDir, "csvTrace" + counter++ + ".csv");
			
			BufferedWriter bw = 
					new BufferedWriter(new FileWriter(traceFile));
			
			Document doc = XMLUtils.convertToDOM(body);
			NodeList events = doc.getElementsByTagName("E");
			
			//LinkedHashMap<String, String> stateVariables = analyseStateVariables(events);
			
			int numEvents = events.getLength();
			for(int i = 0; i < numEvents; i++) {
				NodeList eventObjects = events.item(i).getChildNodes();
				Node eventNode = eventObjects.item(0);
				Node stateNode = eventObjects.item(1);
				NodeList eventFields = eventNode.getChildNodes();
				String eventName = null;
				String targetID = null;
				
				for(int f = 0; f < eventFields.getLength(); f++) {
					if(this.getAttribute(eventFields.item(f), "n").equals("type")) {
						Node v = eventFields.item(f).getFirstChild();
						eventName = this.getAttribute(v, "v");
					} else if(this.getAttribute(eventFields.item(f), "n").equals("targetID")) {
						Node v = eventFields.item(f).getFirstChild();
						targetID = this.getAttribute(v, "v");
					}
					if(eventName != null && ((includeTargetID && targetID != null) || !includeTargetID)) {
						break;
					}
				}
				if(eventName == null || (includeTargetID && targetID == null)) {
					this.log("Could not get event or targetID for a state");
					bw.close();
					return null;
				}
				
				if(includeTargetID) {
					if(targetID.startsWith("\"") && targetID.endsWith("\""))
						targetID = targetID.substring(1, targetID.length() - 1);
					if(eventName.startsWith("\"") && eventName.endsWith("\""))
						eventName = eventName.substring(1, eventName.length() - 1);
					eventName = "\"" + targetID + "_" + eventName + "\"";
				}
				
				//serialize abstract states
				HashMap<String, String> values = new HashMap<String, String>();
				if(writeStates)
					this.getStateValues(stateNode, "", values);
				
				//write to csv
				if(setHeader && writeStates) {
					setHeader = false;
					this.writeCsvLine("eventName", stateVariables, null, bw, true);
				}
				this.writeCsvLine(eventName, stateVariables, values, bw, writeStates);
			}
			
			bw.close();
			csvTraces.add(traceFile.getCanonicalPath());
		}
		
		return csvTraces;
	}
	
	/** API **/
	public void setLogger(Logger logger) {
		this.logger = logger;
	}
	
	public void registerListener(ModelInferenceListener listener) {
		if(listener != null)
			this.listeners.add(listener);
	}
	
	public abstract void startInference(List<Body> traceFiles);
	
	public abstract FSM[] getParetoFront();
}
