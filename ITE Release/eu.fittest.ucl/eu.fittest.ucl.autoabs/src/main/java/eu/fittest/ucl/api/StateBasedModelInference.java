package eu.fittest.ucl.api;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Vector;

import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.MeasureInfeas;
import eu.fbk.weka.MyCluster;
import eu.fittest.itelog.Body;
import eu.fittest.ucl.autoabs.cluster.Cluster;
import eu.fittest.ucl.autoabs.cluster.ClusterAlgo;
import eu.fittest.ucl.autoabs.evolve.Fitness;
import eu.fittest.ucl.autoabs.evolve.InferenceGA;
import eu.fittest.ucl.autoabs.evolve.MultiObjectiveFitness;
import eu.fittest.ucl.autoabs.evolve.Selection;
import eu.fittest.ucl.autoabs.utils.TraceEvent;
import eu.fittest.ucl.autoabs.utils.Utils;

public class StateBasedModelInference extends ModelInference {
		
	private InferenceGA ga;
	private ModelInferenceParameters configuration;
	private MeasureInfeas infeasibilityChecker;
		
	public StateBasedModelInference(Random rand) {
		super(rand);
		this.configuration = new ModelInferenceParameters();
		this.infeasibilityChecker = null;
	}
	public StateBasedModelInference() {
		this(new Random());
	}
		
	private int generateWEKAClusters(Map<Integer, LinkedHashSet<Cluster>> clustering) throws IOException {
		File csv = Utils.saveUniqueSatesToCSV(this.temporaryOutDir, TraceEvent.getTraceHeader());
		File clusterDir = new File(this.temporaryOutDir, "weka");
		int maxK = 0;
		LinkedHashSet<Cluster> clusters;
		//TODO: make max clusters a parameter
		for(int i = 1; i <= 10; i++){
			Utils.emptyDir(clusterDir);
			clusters = new LinkedHashSet<Cluster>();
			MyCluster wekaClusterAlgo = new MyCluster(csv.getCanonicalPath(), clusterDir.getCanonicalPath(), 
					String.valueOf(i));
			try {
				wekaClusterAlgo.run();
			} catch (Exception e) {
				this.log(e.getMessage());
				break;
			}
			maxK = i;
			clusters.addAll(Utils.parseWekaClusters(clusterDir.getCanonicalPath()));
			clustering.put(i, clusters);
			clusters = null;
		}
		return maxK;
	}
	
	private void start(List<Body> traceFiles, boolean includeTargetID) {
		super.start();
		//convert trace files to csv files
		Utils.logger = super.logger;
		Utils.emptyDir(super.temporaryOutDir);
		if(!super.temporaryOutDir.exists())
			super.temporaryOutDir.mkdir();
		
		List<String> csvTraces = null;
		try {
			csvTraces = super.convertLogToCSV(traceFiles, true, includeTargetID);
		} catch (IOException e) {
			super.log(e.getMessage());
			csvTraces = null;
		}
		if(csvTraces == null) {
			super.stop();
			return;
		}
		Utils.clearConcreteStateSet();
		Vector<List<TraceEvent>> events = 
				Utils.computeAllTraceEvents(csvTraces);
		
		if(events.isEmpty()) {
			super.log("Failed to get any trace events");
			super.stop();
			return;
		}
		
		Map<Integer, LinkedHashSet<Cluster>> clustering = new HashMap<Integer, LinkedHashSet<Cluster>>();
		int maxK = 0;
		try {
			maxK = this.generateWEKAClusters(clustering);
		} catch (IOException e) {
			this.log(e.getMessage());
		}
		
		if(maxK <= 1) {
			super.log("failed to generate any WEKA clusters (or only single cluster)");
			super.stop();
			return;
		}
		
		ClusterAlgo cAlgo = new ClusterAlgo(clustering, maxK);
				
		File daikonDataDir = new File(this.temporaryOutDir, "daikon");
		Fitness multiobjectiveFitnessFunction = new MultiObjectiveFitness(daikonDataDir, events, 
				this.configuration.getMaximumInfeasibleSequenceLengthCheck(), 
				this.infeasibilityChecker);
		
		Selection tournamentSelection = new Selection(this.random, 
				this.configuration.getNumTournaments());
		
		this.ga = new InferenceGA(
				super.listeners,
				super.random, multiobjectiveFitnessFunction, tournamentSelection, 
				this.configuration.getPopulationSize(), this.configuration.getMaximumGenerations(), 
				this.configuration.getMaximumGenerationsWithoutProgress(),
				cAlgo, 
				Utils.getConcreteStates(), 
				true, this.configuration.getMutationProbability());
		
		this.ga.run();
		
		File frontDir = new File(super.temporaryOutDir, "paretoFront");
		frontDir.mkdir();
		this.ga.saveParetoFrontToFile(frontDir);
		
		super.stop();
	}
	/** API **/
	@Override
	public void startInference(List<Body> traceFiles) {
		startInference(traceFiles, null);
	}
	public void startInference(List<Body> traceFiles, File tmpOutDir) {
		startInference(traceFiles, tmpOutDir, null);
	}
	public void startInference(List<Body> traceFiles, File tmpOutDir, 
			MeasureInfeas inf) {
		startInference(traceFiles, tmpOutDir, inf, null, false);
	}
	public void startInference(List<Body> traceFiles, File tmpOutDir, 
			MeasureInfeas inf, ModelInferenceParameters configuration, boolean includeTargetID) {
		if(tmpOutDir != null && tmpOutDir.exists() && tmpOutDir.isDirectory())
			super.temporaryOutDir = tmpOutDir;
		if(inf != null)
			this.infeasibilityChecker = inf;
		if(configuration != null)
			this.configuration = configuration;
		
		this.start(traceFiles, includeTargetID);
	}
	
	public FSM[] getParetoFront() {
		if(this.ga == null) return null;
		return this.ga.getNonDominatedFSM();
	}
}
