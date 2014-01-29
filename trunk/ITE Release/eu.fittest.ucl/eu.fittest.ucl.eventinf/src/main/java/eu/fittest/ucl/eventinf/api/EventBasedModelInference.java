package eu.fittest.ucl.eventinf.api;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import eu.fbk.se.fsm.Edge;
import eu.fbk.se.fsm.FSM;
import eu.fittest.ucl.api.ModelInference;
import eu.fittest.ucl.eventinf.evolve.GeneticAlgo;
import eu.fittest.ucl.eventinf.evolve.NSGA2;
import eu.fittest.itelog.Body;

public class EventBasedModelInference extends ModelInference {
	
	private ModelInferenceParameters configuration;
	private GeneticAlgo ga;
	
	public EventBasedModelInference(Random rand) {
		super(rand);
		this.configuration = new ModelInferenceParameters();
	}
	public EventBasedModelInference() {
		this(new Random());
	}
	
	private FSM readTrace(String fileName) {
		FSM fsm = new FSM();
		int nodeId = 0;
		try {
			BufferedReader in = new BufferedReader(new FileReader(fileName));
			String s;
			String curNode = "S" + nodeId++;
			fsm.addNode(curNode);
			fsm.setStartNode(fsm.getNode(curNode));
			String prevNode = curNode;
			//Pattern p = Pattern.compile("\\s*(\\w+)\\s*.*");
			Pattern p = Pattern.compile("\\s*(.+)\\s*.*");
			while ((s = in.readLine()) != null) {
				Matcher m = p.matcher(s);
				if (m.matches()) {
					curNode = "S" + nodeId++;
					fsm.addNode(curNode);
					fsm.getNode(prevNode).addEdge(new Edge(fsm.getNode(curNode), m.group(1)));
					prevNode = curNode;
				}
			}
			
			in.close();
			
		} catch (IOException e) {
			super.log("IO error while reading: " + fileName);
			return null;
		}		
		return fsm;
	}
	
	private void start(List<Body> traceFiles, boolean includeTargetID) {
		super.start();
		Utils.emptyDir(super.temporaryOutDir);
		if(!super.temporaryOutDir.exists())
			super.temporaryOutDir.mkdir();
		List<String> csvTraces = null;
		try {
			csvTraces = this.convertLogToCSV(traceFiles, false, includeTargetID);
		} catch (IOException e) {
			super.log(e.getMessage());
			csvTraces = null;
		}
		if(csvTraces == null) {
			super.stop();
			return;
		}
		
		FSM[] tracesFsm = new FSM[csvTraces.size()];
		for(int i = 0; i < csvTraces.size(); i++) {
			FSM fsm = this.readTrace(csvTraces.get(i));
			if(fsm == null) {
				super.stop();
				return;
			} else {
				tracesFsm[i] = fsm;
			}
		}
		
		File fsmOutputFolder = new File(super.temporaryOutDir, "fsms");
		fsmOutputFolder.mkdir();
		
		this.ga = new NSGA2(super.random, super.listeners, tracesFsm);
		
		this.ga .setPopulationSize(this.configuration.getPopulationSize());
		this.ga .setGenerations(this.configuration.getMaxGenerations());
		this.ga .setOffspringPopulationSize(this.configuration.getOffspringPopulationSize());
		this.ga .setUseMinSize(this.configuration.isUseMinSize());
		this.ga .setMaxGenStringLength(this.configuration.getMaxGenStringLength());
		this.ga .setMutationProbability(this.configuration.getProbMutation());
		this.ga .setCrossoverProbability(this.configuration.getProbCrossover());
		this.ga.setMaxAddTraceAttempts(this.configuration.getMaxAddTraceAttempts());
		this.ga.setMaxFSMSize(this.configuration.getMaxFSMSize());
		this.ga.setProbRandomMerge(this.configuration.getProbRandomMerge());
		try {
			this.ga .run();
		} catch(Exception e) {
			super.log(e.getMessage());
		}
		this.ga.saveNonDominatedToFile(fsmOutputFolder);
		super.stop();
	}
	
	/**** API *****/
	@Override	
	public void startInference(List<Body> traceFiles) {
		startInference(traceFiles, null);
	}
	public void startInference(List<Body> traceFiles, File tmpOutDir) {
		startInference(traceFiles, tmpOutDir, null, false);
	}
	public void startInference(List<Body> traceFiles, File tmpOutDir, 
			ModelInferenceParameters configuration, boolean includeTargetID) {
		if(tmpOutDir != null && tmpOutDir.exists() && tmpOutDir.isDirectory())
			this.temporaryOutDir = tmpOutDir;
		if(configuration != null)
			this.configuration = configuration;
		
		try {
			System.out.println("About to start search-based stuff ...");
			this.start(traceFiles, includeTargetID);
			System.out.println("... finished search-based stuff");
		}
		catch (java.lang.StackOverflowError sofe) {
			System.out.println("Oooooops, stackoverflow!!!");
			sofe.printStackTrace();
		}
	}
	
	public FSM[] getParetoFront() {
		if(this.ga == null) return null;
		return this.ga.getParetoFront().toArray(new FSM[0]);
	}
}
