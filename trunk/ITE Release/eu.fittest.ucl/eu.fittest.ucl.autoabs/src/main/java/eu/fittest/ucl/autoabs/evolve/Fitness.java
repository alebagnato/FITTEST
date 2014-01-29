/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */
package eu.fittest.ucl.autoabs.evolve;

//TODO: add inv diversity as a secondary fitness 

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Vector;

import au.com.bytecode.opencsv.CSVReader;
import eu.fittest.ucl.autoabs.utils.TraceEvent;
import eu.fittest.ucl.autoabs.utils.Utils;
import eu.fittest.ucl.autoabs.cluster.Cluster;
import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;
import daikon.Daikon;
import daikon.PptSlice;
import daikon.PptTopLevel;
//import daikon.ProglangType;
import daikon.ValueTuple;
import daikon.diff.Diff;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.filter.InvariantFilters;
import eu.fbk.se.fsm.Edge;
import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.FSMAlgo;
import eu.fbk.se.fsm.Node;

/**
 * Fitness function, to compare two solutions and
 * write each group of states to a CSV file
 * run convertcsv.pl to generate corresponding decls and dtrace files
 * infer invariants for each csv file
 * each invariant set for a csv file corresponds to a FSM state
 */
public class Fitness implements Comparator<Chromosome>{
	
	
	class FitnessEvaluator implements Runnable {

		private Vector<List<TraceEvent>> events;
		private File outDir;
		private FSM fsm = null;
		private Chromosome ind;
		private boolean forceMultipleClusters;
		
		FitnessEvaluator(Vector<List<TraceEvent>> events, File outDir, Chromosome ind, boolean multiple) {
			this.events = events;
			this.outDir = outDir;
			this.ind = ind;
			this.forceMultipleClusters = multiple;
		}
		
		/**
		 * Helper function to run external convertcsv.pl script that generates
		 * decls and dtrace files
		 * @param csvFile CSV file for which to generate the trace information
		 * @return Array of File objects where [0] == decls file and [1] == dtrace file
		 */
		File[] generateDaikonTraceFiles(File csvFile) {
			//CSVtoDaikonTraceScript csvFile
			/*String name = csvFile.getName();
			String[] command = {"perl", "-I", PLUME_LIB_BIN, CSV_TO_TRACE_SCRIPT, name};
			String prefix = stripCSVSuffix(name);
			String decls = prefix + ".decls";
			String dtrace = prefix + ".dtrace";
			String dir = csvFile.getParent() + File.separator;
			
			if(!Utils.executeProcess(command, dir))
				System.exit(1);
			
			return new File[]{new File(dir + decls), new File(dir + dtrace)};*/
			
			String name = csvFile.getName();
			String prefix = stripCSVSuffix(name);
			String decls = prefix + ".decls";
			String dtrace = prefix + ".dtrace";
			String dir = csvFile.getParent() + File.separator;
			
			File fDecls = new File(dir + decls);
			File fDtrace = new File(dir + dtrace);
			
			//TODO: generate custom dtrace and decls files
			CSVReader reader = null;
			BufferedWriter declWriter = null;
			BufferedWriter traceWriter = null;
			
			String [] nextLine;
			String[] columns = null;
			int[] types = null;
			//boolean decs = false;
			try {
				reader = new CSVReader(new FileReader(csvFile));
				traceWriter = new BufferedWriter(new FileWriter(fDtrace));
				while ((nextLine = reader.readNext()) != null) {
					if(columns == null) {
						columns = nextLine;
						types = new int[columns.length];
						for(int i = 0; i < types.length; i++) {
							types[i] = -1;
						}
					} else {
						if(declWriter == null) {
							declWriter = new BufferedWriter(new FileWriter(fDecls));
							declWriter.write("DECLARE" + Utils.eol);
							declWriter.write("aprogram.point:::POINT" + Utils.eol);
						}
						traceWriter.write("aprogram.point:::POINT" + Utils.eol);
						for(int i = 0; i < nextLine.length; i++) {
							String item = nextLine[i];
							BigDecimal _d = Utils.isDouble(item);
							if(types[i] == -1) {
								if(_d != null)
									types[i] = 1;
								else
									types[i] = 0;
							}
							
							traceWriter.write(columns[i] + Utils.eol);
							
							if(types[i] == 1) {
								traceWriter.write(item + Utils.eol);
								if(declWriter != null) {
									declWriter.write(columns[i] + Utils.eol);
									declWriter.write("double" + Utils.eol);
									declWriter.write("double" + Utils.eol);
									declWriter.write("1" + Utils.eol);
								}
							} else {
								if(item.startsWith("\"") && item.endsWith("\""))
									traceWriter.write(item + Utils.eol);
								else {
									if(item.startsWith("'") && item.endsWith("'"))
										item = item.substring(1, item.length() - 1);
									traceWriter.write("\"" + item + "\"" + Utils.eol);
								}
								if(declWriter != null) {
									declWriter.write(columns[i] + Utils.eol);
									declWriter.write("java.lang.String" + Utils.eol);
									declWriter.write("java.lang.String" + Utils.eol);
									declWriter.write("1" + Utils.eol);
								}
							}
							traceWriter.write("1" + Utils.eol);
						}
						traceWriter.write("" + Utils.eol);
						if(declWriter != null) {
							declWriter.close();
							declWriter = null;
						}
						//decs = true;
					}
				}
			} catch (IOException e) {
				e.printStackTrace();
				System.exit(1);
			}finally {
				if(reader != null) {
					try {
						reader.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
				if(declWriter != null) {
					try {
						declWriter.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
					declWriter = null;
				}
				if(traceWriter != null) {
					try{
						traceWriter.close();
					} catch(IOException e) {
						e.printStackTrace();
					}
				}
			}
			
			return new File[]{fDecls, fDtrace};
		}
		
		protected FSM buildFSM(Chromosome ind, Map<Integer, List<Invariant>> clusterInvMap) {
			String startLabel = "START";
			FSM fsm = new FSM();
			fsm.addNode(startLabel);
			Node currentNode = fsm.getNode(startLabel);
			fsm.setStartNode(currentNode);
			
			ConcreteState state = null;
			String startNodeAlias;
			
			for(List<TraceEvent> eventList : this.events) {
				//set of current states we're in
				List<Node> currentStates = new ArrayList<Node>();
				currentStates.add(currentNode);
				startNodeAlias = null;
				
				for(TraceEvent event : eventList) {
					state = event.getConcreteState();
					String eventName = event.getEventName();
					if(eventName.equals(startLabel)) {
						startNodeAlias = "S" + ind.getClusterId(state);
						continue;
					}
					Set<Integer> clusterIds = getSatisfiedInvariantSets(state, clusterInvMap);
					if(clusterIds.size() == 0) {
						throw new RuntimeException("missing target state for transition");
					}
					List<Node> newCurrentStates = new ArrayList<Node>(clusterIds.size());
					for(int id : clusterIds) {
						String stateName = "S" + id;
						Node target;
						if(startNodeAlias != null && stateName.equals(startNodeAlias)) {
							stateName = startLabel;//fsm.getStartNode();
						} 
						target = fsm.getNode(stateName);
						if(target == null) {
							//add the state
							target = new Node(stateName);
							fsm.addNode(stateName, target);
						}
						newCurrentStates.add(target);
						for(Node sourceNode : currentStates){
							sourceNode.addEdge(new Edge(target, eventName));
						}
					}
					currentStates.clear();
					currentStates = newCurrentStates;
				}
				
				currentNode = fsm.getStartNode();
			}
			
			state = null;
			return fsm;
		}
		
		/**
		 * Combine all states (i.e. merge ids in the Chromosome) that have identical invariants 
		 * @param ind Chromosome
		 * @param invariants List of invariants inferred by Daikon
		 */
		protected void combineClustersWithIdenticalInv(Chromosome ind, Map<Integer, List<Invariant>> clusterInvMap){
			List<Set<Integer>> equalClusters = new ArrayList<Set<Integer>>();
			Set<Entry<Integer, List<Invariant>>> entries = clusterInvMap.entrySet();
			for(Entry<Integer, List<Invariant>> mapping : entries){
				int clusterId = mapping.getKey();
				List<Invariant> inv1list = mapping.getValue();
				Set<Integer> equal = new HashSet<Integer>();
				for(Entry<Integer, List<Invariant>> mapping2 : entries) {
					if(clusterId == mapping2.getKey()) continue; //same cluster
					boolean merge = true;
					List<Invariant> inv2list = mapping2.getValue();
					if(inv1list.size() != inv2list.size()) {
						merge = false;
					} else {
						//use two loops, in case inv1list and inv2list are in different order, but contain
						//the same invariants
						int n = inv1list.size();
						for(int i = 0; i < n; i++) {
							boolean found = false;
							Invariant i1 = inv1list.get(i);
							for(int j = 0; j < n; j++) {
								Invariant i2 = inv2list.get(j);
								if(i1.isSameInvariant(i2)) {
									found = true;
									break;
								}
							}
							if(!found) {
								merge = false;
								break;
							}
							/*if(!inv1list.get(i).isSameInvariant(inv2list.get(i))) {
								merge = false;
								break;
							}*/
						}
					}
					if(merge) {
						equal.add(clusterId);
						equal.add(mapping2.getKey());
					}
				}
				equalClusters.add(equal);
			}
			//update clustering so that cluster with the same invariants are merged
			for(Set<Integer> equal : equalClusters) {
				ind.mergeStates(equal);
			}
			entries = null;
			equalClusters.clear();
			equalClusters = null;
		}
		
		/**
		 * Compute the invariants for each group of states
		 * @param clusters Collection of cluster (files)
		 * @return A mapping for each cluster to its invariants
		 */
		public Map<Integer, List<Invariant>> computeAllClusterInvariants(Collection<File> clusters){
			Map<Integer, List<Invariant>> clusterInvMap = new HashMap<Integer, List<Invariant>>();
			for(File f : clusters) {
				//name of cluster.csv file
				String fileName = f.getName();
				//strip clusterid from cluster_id.csv file
				int clusterId = Integer.parseInt(fileName.substring(8, fileName.length() - 4));
				
				//generate the decls and dtrace files for a cluster
				File[] traceFiles = this.generateDaikonTraceFiles(f);
				
				File invFile = new File(traceFiles[0].getParent(), "cluster_" + clusterId + ".inv.gz");
				
				//somtimes there seems to be a sporadic error in reading a dtrace file, even though the file looks fine,
				//so I'll add a re-try attempt in case the first read fails. Try a maximum of 3 times
				int numTries = 0;
				int maxTries = 3;
				while(numTries < maxTries) {
					try{
						String[] args = {"-o", invFile.getCanonicalPath(), "--nohierarchy", 
								traceFiles[0].getCanonicalPath(),
								traceFiles[1].getCanonicalPath()};
							
						System.out.println("infering invariants for:" + f.getName());
						
						Daikon.cleanup();
						//infer invariants
						Daikon.mainHelper(args);
						
						for (PptTopLevel ppt : Daikon.all_ppts.all_ppts()) {
							for (Iterator<PptSlice> i = ppt.views_iterator(); i.hasNext();) {
								PptSlice slice = i.next();
								for (Invariant inv : slice.invs) {
									if(inv.getConfidence() < Invariant.dkconfig_confidence_limit)
										continue;
									else if(INV_FILTERS.shouldKeep(inv) == null)
										continue;
									else {
										List<Invariant> invariants = clusterInvMap.get(clusterId);
										if(invariants == null){
											invariants = new ArrayList<Invariant>();
											invariants.add(inv);
											clusterInvMap.put(clusterId, invariants);
										} else 
											invariants.add(inv);
									}
								}
							}
						}
						
						break;
					} catch(IOException e) {
						numTries++;
					}
				}
				
				if(numTries >= maxTries) {
					System.out.println("could not run Daikon.");
					System.exit(1);
				}
				
				traceFiles = null;
				invFile = null;
			}
			return clusterInvMap;
		}
		
		@Override
		public void run() {
			Collection<Cluster> clusters = this.ind.generateClusters();
			int numClusters = clusters.size();
			if(numClusters == 1 && !this.forceMultipleClusters) {
				return;
			}
			
			Collection<File> csvFilesForDaikon = Utils.saveClustersForDaikon(this.outDir, this.events, clusters);
			for(Cluster c : clusters) {
				c.dispose();
			}
			clusters.clear();
			clusters = null;
			
			Map<Integer, List<Invariant>> clusterInvMap = this.computeAllClusterInvariants(csvFilesForDaikon);
			
			//merge clusters that have the same invariants
			this.combineClustersWithIdenticalInv(ind, clusterInvMap);
			
			this.fsm = this.buildFSM(ind, clusterInvMap);
			
		}
		
		public FSM getFSM() {
			return this.fsm;
		}
		
		public void dispose() {
			this.fsm = null;
			this.ind = null;
			this.outDir = null;
			this.events = null;
			Daikon.cleanup();
			Daikon.all_ppts.all_ppts().clear();
			Daikon.proto_invs.clear();
		}
	}
	
	protected final File outDir;
	protected static int mergeNodesK = 5;
	protected static boolean mergeTails = false;
	
	private final FSMAlgo fsmAlgo;
	
	private final Diff daikonDiff;
	private final Comparator<Invariant> comparator;
	private final static InvariantFilters INV_FILTERS = InvariantFilters.defaultFilters();
		
	protected final Vector<List<TraceEvent>> events;
	
	public static void mergeTails(boolean merge) {
		mergeTails = merge;
	}
	public static void setKTailLength(int length) {
		mergeNodesK = length;
	}
	public Fitness(File outDir, Vector<List<TraceEvent>> events) {
		this.outDir = outDir;
		this.comparator = new Invariant.ClassVarnameFormulaComparator();
		this.daikonDiff = new Diff(false, false);
		this.daikonDiff.setAllInvComparators(this.comparator);
		this.events = events;
		this.fsmAlgo = new FSMAlgo();
	}
	
	public int compare(Chromosome o1, Chromosome o2) {
		if(o1.getFitness() == o2.getFitness())
			return 0;
		else if(o1.getFitness() < o2.getFitness())
			return -1;
		else
			return 1;
	}
	
	public boolean areEqual(Chromosome o1, Chromosome o2) {
		return o1.getFitness() == o2.getFitness();
	}

	/**
	 * TODO: make sure I count non-deterministic edges, not # of non-determinism
	 * Count the number of non-deterministic transitions in an FSM by examining
	 * all nodes and their outgoing edges
	 * @param fsm FSM to check
	 * @return Count of non-deterministic transitions
	 */
	public int countNonDeterministicEvents(FSM fsm) {
		int count = 0;
		Collection<Node> allNodes = fsm.getNodes();
		for(Node n : allNodes){
			Map<String, List<String>> transitionMap = new HashMap<String, List<String>>();
			for(Edge e : n.getSucc()) {
				String eventName = e.getEvent();
				String targetName = e.getTarget().getLabel();
				if(transitionMap.containsKey(eventName)) {
					List<String> targets = transitionMap.get(eventName);
					if(!targets.contains(targetName)) {
						count++;
					}
					targets.add(targetName);
				} else {
					List<String> targets = new ArrayList<String>(1);
					targets.add(targetName);
					transitionMap.put(eventName, targets);
				}
			}
			transitionMap.clear();
			transitionMap = null;
		}
		return count;
	}
	
	/**
	 * Helper function to remove .csv ending from a file path, e.g.
	 * test.csv -> test
	 * @param file Filename
	 * @return Filename without .csv
	 */
	private static String stripCSVSuffix(String file) {
		return file.substring(0, file.length() - 4);
	}
	
	
	/**This function returns the cluster id for a state (label)
	 * 
	 * @param label The label of the state whose cluster id to retrieve
	 * @return A set of cluster ids
	 */
	private Set<Integer> getClusterIdsOfState(String label) {
		Set<Integer> iids = new HashSet<Integer>();
		String[] ids = label.split("_");
		if(ids == null || ids.length == 0) {
			//remove 'S'
			if(!label.toLowerCase().equals("start"))
				iids.add(Integer.valueOf(label.substring(1)));
		} else {
			for(int i = 0; i < ids.length; i++) {
				if(!ids[i].toLowerCase().equals("start")) {
					iids.add(Integer.valueOf(ids[i].substring(1)));
				}
			}
		}
		return iids;
	}
	/** This function merges all pairs of nodes that share the same k-tail
	 * @param fsm The FSM whose node pairs to check
	 * @param ind The chromsome from which them FSM was built
	 */
	protected void kTailMerge(FSM fsm, Chromosome ind) {
		boolean merged;
		while(true) {
			merged = false;
			Collection<Node> nodes = fsm.getNodes();
			outer:
			for(Node n1 : nodes) {
				for(Node n2 : nodes) {
					if(n1.getLabel().equals(n2.getLabel())) continue;
					//check k-tail
					if(this.fsmAlgo.checkTail(n1, n2, mergeNodesK)) {
						//extract cluster ids
						Set<Integer> toMerge = new HashSet<Integer>();
						toMerge.addAll(this.getClusterIdsOfState(n1.getLabel()));
						toMerge.addAll(this.getClusterIdsOfState(n2.getLabel()));
						//persist changes in chromosome too
						ind.mergeStates(toMerge);
						this.fsmAlgo.merge(fsm, n1, n2);
						merged = true;
						break outer;
					}
				}
			}
			if(!merged)
				break;
		}
	}
	
	/** This function gets all invariant (sets) that are satisifed by the state. 
	 * An invariant set denotes all the invariants inferred for one clustering of states
	 * @param state A concrete state
	 * @return A set of cluster ids that have invariants inferred which hold for the passed in state
	 */
	private Set<Integer> getSatisfiedInvariantSets(ConcreteState state, 
			Map<Integer, List<Invariant>> clusterInvMap){
		
		Set<Integer> clusters = new HashSet<Integer>();
		
		Object[] vals = state.getValues();
		//change BigDecimal to Double, otherwise I get an excpetion in
		//singlefloat
		for(int i = 0; i < vals.length; i++) {
			if(vals[i] instanceof BigDecimal)
				vals[i] = (Double)((BigDecimal)vals[i]).doubleValue();
		}
		
		int[] mods = new int[vals.length];
		for(int j = 0; j < mods.length; j++) mods[j] = 1;
		
		ValueTuple vt = new ValueTuple(vals, mods);
		//now go through all invariants from all clusters and check if it accepts that vt
		for(Entry<Integer, List<Invariant>> entry : clusterInvMap.entrySet()) {
			int currentClusterId = entry.getKey();
			List<Invariant> invs = entry.getValue();
			boolean reject = false;
			for(Invariant inv : invs) {
				Invariant clone = inv.clone();
				try{
					InvariantStatus status = clone.add_sample(vt, 1);
					if(status != InvariantStatus.NO_CHANGE) {
						//reject
						//System.out.println("violation: " + inv.toString() + ", " + status.toString());
						reject = true;
						break;
					}
				} catch(Exception e) {
					System.out.println(e.getMessage());
					System.out.println("-----");
					System.out.println(state.toString());
					System.out.println("----");
					System.out.println(inv.toString());
					//System.exit(1);
					reject = true;
					break;
				}
			}
			if(!reject) {
				clusters.add(currentClusterId);
			}
		}
		mods = null;
		vals = null;
		vt = null;
		return clusters;
	}
	
			
	public void evaluate(Chromosome ind)
	{
		this.evaluate(ind, false);
	}
	
	public void evaluate(Chromosome ind, boolean force) {
		/*Collection<Cluster> clusters = ind.generateClusters();
		int numClusters = clusters.size();
		if(numClusters == 1 && !force) {
			//penalize because we only have a single state (and start state)
			ind.setFSM(null);
			ind.setFitness(Integer.MAX_VALUE);
			return;
		}
		
		Collection<File> csvFilesForDaikon = Utils.saveClustersForDaikon(this.outDir, this.events, clusters);
		for(Cluster c : clusters) {
			c.dispose();
		}
		clusters.clear();
		clusters = null;
		Map<Integer, List<Invariant>> clusterInvMap = this.computeAllClusterInvariants(csvFilesForDaikon);
		
		//merge clusters that have the same invariants
		this.combineClustersWithIdenticalInv(ind, clusterInvMap);
		
		FSM fsm = this.buildFSM(ind, clusterInvMap);
		
		//cleanup
		Set<Entry<Integer, List<Invariant>>> entries = clusterInvMap.entrySet();
		for(Entry<Integer, List<Invariant>> mapping : entries) {
			mapping.getValue().clear();
		}
		clusterInvMap.clear();
		clusterInvMap = null;
		
		Daikon.cleanup();
		*/
		
		FitnessEvaluator fitEvaluator = new FitnessEvaluator(this.events, this.outDir, ind, force); 
		Thread t = new Thread(fitEvaluator);
		t.start();
		try {
			t.join();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		FSM fsm = fitEvaluator.getFSM();
		fitEvaluator.dispose();
		fitEvaluator = null;
		t = null;
		
		if(fsm == null) {
			ind.setFSM(null);
			ind.setFitness(Integer.MAX_VALUE);
			return;
		}
		
		if(mergeTails){
			//merge nodes that share the same k-tail
			this.kTailMerge(fsm, ind);
		}
		
		if(fsm.getNodes().size() == 1 && !force) {
			ind.setFSM(null);
			ind.setFitness(Integer.MAX_VALUE);
			return;
		}
		
		ind.setFSM(fsm);
		
		ind.setFitness(this.countNonDeterministicEvents(fsm));
	}
}
